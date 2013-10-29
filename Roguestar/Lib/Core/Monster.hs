{-# LANGUAGE TypeFamilies, PatternGuards #-}
--Core
module Roguestar.Lib.Core.Monster
    (generateInitialPlayerMonster,
     newMonster,
     getMonsterSpecial,
     getMonsterAbilityScore,
     getCurrentMonster,
     getMonsterFaction,
     injureMonster,
     healMonster,
     getMonsterHealth,
     getDead,
     deleteMonster,
     sweepDead)
    where

import Prelude hiding (getContents)
import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.DB
import Roguestar.Lib.Data.SpeciesData
import Roguestar.Lib.Data.SpeciesTraits
import Roguestar.Lib.Data.FactionData
import Control.Monad.Error
import Control.Monad.Random
import Control.Monad.Reader
import Roguestar.Lib.Data.FacingData
import Roguestar.Lib.Position
import Roguestar.Lib.Core.Plane
import Roguestar.Lib.Data.PlayerState
import Roguestar.Lib.Utility.DetailedLocation
import Roguestar.Lib.Logging
import qualified Data.Set as Set

-- |
-- Generates a new Monster from the specified species.
--
generateMonster :: Faction -> Species -> DB Monster
generateMonster faction species =
    do r <- getRandomR (1,1000000)
       return $ applyToMonster (species_specials $ speciesInfo species) $
                applyToMonster (species_traits $ speciesInfo species) $
           empty_monster {
               creature_species = species,
               creature_faction = faction,
               creature_random_id = r }

-- |
-- During DBRaceSelectionState, generates a new Monster for the player character.
--
generateInitialPlayerMonster :: Species -> DB ()
generateInitialPlayerMonster species =
    do newc <- generateMonster Player species
       setPlayerState $ SpeciesSelectionState $ Just newc

-- |
-- Generates a new Monster from the specified Species and adds it to the database.
--
newMonster :: (LocationConstructor l, ReferenceTypeOf l ~ Monster) => Faction -> Species -> l -> DB MonsterRef
newMonster faction species loc =
    do creature <- generateMonster faction species
       dbAddMonster creature loc

getMonsterSpecial :: (DBReadable db) => MonsterSpecial -> MonsterRef -> db Bool
getMonsterSpecial special creature_ref = liftM (Set.member special . creature_specials) $ asks $ getMonster creature_ref

getMonsterAbilityScore :: (DBReadable db) => MonsterAbility -> MonsterRef -> db Integer
getMonsterAbilityScore ability creature_ref =
    do raw_ideal <- liftM (creatureAbilityScore ability) $ asks $ getMonster creature_ref
       terrain_ideal <- getTerrainAffinity creature_ref
       return $ raw_ideal + terrain_ideal

-- | Ability bonus based on being good at working on specific types of terrain.
getTerrainAffinity :: (DBReadable db) => MonsterRef -> db Integer
getTerrainAffinity creature_ref =
    do (Parent plane_ref,pos) <- liftM detail $ getPlanarLocation creature_ref
       terrain_affinity_points <- liftM sum $ forM [minBound..maxBound] $ \face ->
               do t <- terrainAt plane_ref $ offsetPosition (facingToRelative face) pos
                  liftM (creatureAbilityScore $ TerrainAffinity t) $ asks $ getMonster creature_ref
       return $ terrain_affinity_points `div` 4

-- | Get the current creature, if it belongs to the specified faction, based on the current playerState.
getCurrentMonster :: (DBReadable db) => Faction -> db (Maybe MonsterRef)
getCurrentMonster faction =
    do m_who <- liftM subjectOf $ playerState
       is_one_of_us <- maybe (return False) (liftM (== faction) . getMonsterFaction) m_who
       return $ if is_one_of_us then m_who else Nothing

getMonsterFaction :: (DBReadable db) => MonsterRef -> db Faction
getMonsterFaction = liftM creature_faction . asks . getMonster

injureMonster :: Integer -> MonsterRef -> DB ()
injureMonster x = dbModMonster $ \c -> c { creature_damage = max 0 $ creature_damage c + x }

healMonster :: Integer -> MonsterRef -> DB ()
healMonster = injureMonster . negate

-- | Health as a fraction of 1.
getMonsterHealth :: (DBReadable db) => MonsterRef -> db MonsterHealth
getMonsterHealth creature_ref = liftM creatureHealth $ asks $ getMonster creature_ref

getDead :: (DBReadable db) => Reference a -> db [MonsterRef]
getDead parent_ref = filterRO (liftM ((<= 0) . creature_health) . getMonsterHealth) =<< liftM asChildren (asks $ getContents parent_ref)

deleteMonster :: MonsterRef -> DB ()
deleteMonster creature_ref =
    do logDB gameplay_log INFO $ "deleteMonster; creature=" ++ show (toUID creature_ref)
       planar <- liftM identityDetail $ getPlanarLocation creature_ref
       dbUnsafeDeleteObject creature_ref $ const $ return planar

-- | Delete all dead creatures from the database.
sweepDead :: Reference a -> DB ()
sweepDead ref =
    do logDB gameplay_log INFO "sweepDead; sweeping dead creatures"
       worst_to_best_critters <- sortByRO (liftM creature_health . getMonsterHealth) =<< getDead ref
       flip mapM_ worst_to_best_critters $ \creature_ref ->
           do dbPushSnapshot (KilledEvent creature_ref)
              deleteMonster creature_ref
