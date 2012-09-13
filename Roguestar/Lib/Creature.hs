{-# LANGUAGE TypeFamilies, PatternGuards #-}

module Roguestar.Lib.Creature
    (generateInitialPlayerCreature,
     newCreature,
     Roll(..),
     RollComponents(..),
     rollCreatureAbilityScore,
     getCurrentCreature,
     getCreatureFaction,
     injureCreature,
     healCreature,
     getCreatureHealth,
     getDead,
     deleteCreature,
     sweepDead)
    where

import Prelude hiding (getContents)
import Roguestar.Lib.CreatureData
import Roguestar.Lib.DB
import Roguestar.Lib.SpeciesData
import Roguestar.Lib.Species
import Roguestar.Lib.FactionData
import Control.Monad.Error
import Control.Monad.Random
import Roguestar.Lib.Tool
import Data.Monoid
import Data.Ratio
import Roguestar.Lib.Facing
import Roguestar.Lib.Position
import Roguestar.Lib.Plane
import Roguestar.Lib.PlayerState
import Roguestar.Lib.DetailedLocation
import Roguestar.Lib.Logging

-- |
-- Generates a new Creature from the specified species.
--
generateCreature :: Faction -> Species -> DB Creature
generateCreature faction species =
    do r <- getRandomR (1,1000000)
       return $ applyToCreature (species_traits $ speciesInfo species) $  empty_creature {
           creature_species = species,
           creature_faction = faction,
           creature_random_id = r }

-- |
-- During DBRaceSelectionState, generates a new Creature for the player character.
--
generateInitialPlayerCreature :: Species -> DB ()
generateInitialPlayerCreature species =
    do newc <- generateCreature Player species
       setPlayerState $ SpeciesSelectionState $ Just newc

-- |
-- Generates a new Creature from the specified Species and adds it to the database.
--
newCreature :: (LocationConstructor l, ReferenceTypeOf l ~ Creature) => Faction -> Species -> l -> DB CreatureRef
newCreature faction species loc =
    do creature <- generateCreature faction species
       dbAddCreature creature loc

data RollComponents = RollComponents {
    component_base :: Integer,
    component_other_situation_bonus :: Integer,
    component_terrain_affinity_bonus :: Integer }
        deriving (Show)
data Roll = Roll {
    roll_ideal :: Integer,
    roll_actual :: Integer,
    roll_ideal_components :: RollComponents,
    roll_actual_components :: RollComponents,
    roll_log :: Integer }
        deriving (Show)

rollCreatureAbilityScore :: (DBReadable db) => CreatureAbility -> Integer -> CreatureRef -> db Roll
rollCreatureAbilityScore score other_ideal creature_ref =
    do raw_ideal <- liftM (creatureAbilityScore score) $ dbGetCreature creature_ref
       terrain_ideal <- getTerrainAffinity creature_ref
       let ideal = raw_ideal + other_ideal + terrain_ideal
       actual <- linearRoll ideal
       [raw_actual, other_actual, terrain_actual] <- fixedSumLinearRoll [raw_ideal, other_ideal, terrain_ideal] actual
       logarithmic <- logRoll ideal
       let result = Roll ideal (if raw_actual == 0 then 0 else actual)
                        (RollComponents raw_ideal other_ideal terrain_ideal)
                        (RollComponents raw_actual other_actual terrain_actual) logarithmic
       logDB log_creature DEBUG $ "rollCreatureAbilityScore; result=" ++ show result
       return result

-- | Ability bonus based on being good at working on specific types of terrain.
getTerrainAffinity :: (DBReadable db) => CreatureRef -> db Integer
getTerrainAffinity creature_ref =
    do (Parent plane_ref,pos) <- liftM detail $ getPlanarLocation creature_ref
       terrain_affinity_points <- liftM sum $ forM [minBound..maxBound] $ \face ->
               do t <- terrainAt plane_ref $ offsetPosition (facingToRelative face) pos
                  liftM (creatureAbilityScore $ TerrainAffinity t) $ dbGetCreature creature_ref
       return $ terrain_affinity_points `div` 4

-- | Get the current creature, if it belongs to the specified faction, based on the current playerState.
getCurrentCreature :: (DBReadable db) => Faction -> db (Maybe CreatureRef)
getCurrentCreature faction =
    do m_who <- liftM subjectOf $ playerState
       is_one_of_us <- maybe (return False) (liftM (== faction) . getCreatureFaction) m_who
       return $ if is_one_of_us then m_who else Nothing

getCreatureFaction :: (DBReadable db) => CreatureRef -> db Faction
getCreatureFaction = liftM creature_faction . dbGetCreature

injureCreature :: Integer -> CreatureRef -> DB ()
injureCreature x = dbModCreature $ \c -> c { creature_damage = max 0 $ creature_damage c + x }

healCreature :: Integer -> CreatureRef -> DB ()
healCreature = injureCreature . negate

-- | Health as a fraction of 1.
getCreatureHealth :: (DBReadable db) => CreatureRef -> db CreatureHealth
getCreatureHealth creature_ref = liftM creatureHealth $ dbGetCreature creature_ref

getDead :: (DBReadable db) => Reference a -> db [CreatureRef]
getDead parent_ref = filterRO (liftM ((<= 0) . creature_health) . getCreatureHealth) =<< liftM asChildren (getContents parent_ref)

deleteCreature :: CreatureRef -> DB ()
deleteCreature creature_ref =
    do logDB log_creature INFO $ "deleteCreature; creature=" ++ show (toUID creature_ref)
       planar <- liftM identityDetail $ getPlanarLocation creature_ref
       dbUnsafeDeleteObject creature_ref $ const $ return planar

-- | Delete all dead creatures from the database.
sweepDead :: Reference a -> DB ()
sweepDead ref =
    do logDB log_creature INFO "sweepDead; sweeping dead creatures"
       worst_to_best_critters <- sortByRO (liftM creature_health . getCreatureHealth) =<< getDead ref
       flip mapM_ worst_to_best_critters $ \creature_ref ->
           do dbPushSnapshot (KilledEvent creature_ref)
              deleteCreature creature_ref
