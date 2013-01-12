{-# LANGUAGE TypeFamilies, PatternGuards #-}
--Core
module Roguestar.Lib.Creature
    (generateInitialPlayerCreature,
     newCreature,
     getCreatureSpecial,
     getCreatureAbilityScore,
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
import Roguestar.Lib.Facing
import Roguestar.Lib.Position
import Roguestar.Lib.Core.Plane
import Roguestar.Lib.Data.PlayerState
import Roguestar.Lib.DetailedLocation
import Roguestar.Lib.Logging
import qualified Data.Set as Set

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

getCreatureSpecial :: (DBReadable db) => CreatureSpecial -> CreatureRef -> db Bool
getCreatureSpecial special creature_ref = liftM (Set.member special . creature_specials) $ dbGetCreature creature_ref

getCreatureAbilityScore :: (DBReadable db) => CreatureAbility -> CreatureRef -> db Integer
getCreatureAbilityScore ability creature_ref =
    do raw_ideal <- liftM (creatureAbilityScore ability) $ dbGetCreature creature_ref
       terrain_ideal <- getTerrainAffinity creature_ref
       return $ raw_ideal + terrain_ideal

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
