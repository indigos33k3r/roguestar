{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}
--Mechanics
module Roguestar.Lib.Turns
    (performPlayerTurn)
    where

import Prelude hiding (getContents)
import Control.Monad.Maybe
import Control.Monad.Trans
import Roguestar.Lib.DB
import Roguestar.Lib.Reference
import Roguestar.Lib.Data.FactionData
import Roguestar.Lib.Data.SpeciesData
import Roguestar.Lib.Data.MonsterData (Monster)
import Roguestar.Lib.Core.Plane
import Control.Monad
import Roguestar.Lib.Core.Monster
import Data.Ratio
import Roguestar.Lib.Data.FacingData
import Roguestar.Lib.Data.TerrainData
import Data.Maybe
import Roguestar.Lib.Behavior
import qualified Roguestar.Lib.Perception as P
import Roguestar.Lib.Position
import Roguestar.Lib.Data.PlayerState
import Roguestar.Lib.Logging
import Roguestar.Lib.Utility.DetailedLocation
import Control.Monad.Random
import Data.List as List

performPlayerTurn :: Behavior -> MonsterRef -> DB ()
performPlayerTurn beh creature_ref =
    do logDB gameplay_log INFO $ "performPlayerTurn; Beginning player action: " ++ show beh
       executeBehavior beh creature_ref
       logDB gameplay_log INFO $ "performPlayerTurn; Doing AI turns:"
       dbFinishPendingAITurns
       logDB gameplay_log INFO $ "performPlayerTurn; Finished all player and AI turns."

dbFinishPendingAITurns :: DB ()
dbFinishPendingAITurns =
    do m_current_plane <- getCurrentPlane
       case m_current_plane of
           Just p -> dbFinishPlanarAITurns p
           Nothing -> return ()

dbFinishPlanarAITurns :: PlaneRef -> DB ()
dbFinishPlanarAITurns plane_ref =
    do logDB gameplay_log INFO $ "Running turns for plane: id=" ++ show (toUID plane_ref)
       sweepDead plane_ref
       (all_creatures_on_plane :: [MonsterRef]) <- liftM asChildren $ getContents plane_ref
       any_players_left <- liftM (any (== Player)) $ mapM getMonsterFaction all_creatures_on_plane
       next_turn <- dbNextTurn $ List.map genericReference all_creatures_on_plane ++ [genericReference plane_ref]
       case next_turn of
           _ | not any_players_left ->
               do logDB gameplay_log INFO $ "dbFinishPlanarAITurns; Game over condition detected"
                  setPlayerState $ GameOver PlayerIsDead
                  return ()
           ref | ref =:= plane_ref ->
               do dbPerform1PlanarAITurn plane_ref
                  dbFinishPlanarAITurns plane_ref
           ref | Just creature_ref <- coerceReference ref ->
               do faction <- getMonsterFaction creature_ref
                  if (faction /= Player)
                      then do dbPerform1MonsterAITurn creature_ref
                              dbFinishPlanarAITurns plane_ref
                      else setPlayerState (PlayerMonsterTurn creature_ref)
                  return ()
           _ -> error "dbFinishPlanarAITurns: impossible case"

planar_turn_interval :: Rational
planar_turn_interval = 1%2

monster_spawns :: [(Terrain,Species)]
monster_spawns = [(RecreantFactory,RedRecreant)]

dbPerform1PlanarAITurn :: PlaneRef -> DB ()
dbPerform1PlanarAITurn plane_ref =
    do logDB gameplay_log INFO $ "dbPerform1PlanarAITurn; Beginning planar AI turn (for the plane itself):"
       (creature_locations :: [DetailedLocation (Child Monster)]) <- liftM mapLocations $ getContents plane_ref
       player_locations <- filterRO (liftM (== Player) . getMonsterFaction . asChild . detail) creature_locations
       num_npcs <- liftM length $ filterRO (liftM (/= Player) . getMonsterFaction . asChild . detail) creature_locations
       when (num_npcs < length player_locations * 3) $
           do (terrain_type,species) <- weightedPickM $ unweightedSet monster_spawns
              _ <- spawnNPC terrain_type species plane_ref $ List.map detail $ player_locations
              return ()
       dbAdvanceTime plane_ref planar_turn_interval

-- |
-- Spawn a non-player creature on the specified terrain type (or fail if not finding that terrain type)
--
spawnNPC :: Terrain -> Species -> PlaneRef -> [Position] -> DB Bool
spawnNPC terrain_type species plane_ref player_locations =
    do logDB gameplay_log INFO $ "spawnNPC; Spawning an NPC"
       p <- weightedPickM $ unweightedSet player_locations
       m_spawn_position <- pickRandomClearSite_withTimeout (Just 2) 7 0 0 p (== terrain_type) plane_ref
       case m_spawn_position of
           Nothing -> return False
           Just spawn_position ->
               do new_creature <- newMonster Monsters species (Standing plane_ref spawn_position Here)
                  dbPushSnapshot (SpawnEvent new_creature)
                  return True

dbPerform1MonsterAITurn :: MonsterRef -> DB ()
dbPerform1MonsterAITurn creature_ref =
    do logDB gameplay_log INFO $ "dbPerform1MonsterAITurn; Performing a creature's AI turn: id=" ++ show (toUID creature_ref)
       liftM (const ()) $ atomic (flip executeBehavior creature_ref) $ P.runPerception creature_ref $ liftM (fromMaybe Vanish) $ runMaybeT $
        do let isPlayer :: forall db. (DBReadable db) => Reference () -> P.DBPerception db Bool
               isPlayer ref | (Just might_be_the_player_creature_ref) <- coerceReference ref =
                   do f <- P.getMonsterFaction might_be_the_player_creature_ref
                      return $ f == Player
               isPlayer _ | otherwise = return False
           (visible_player_locations :: [Position]) <- lift $ liftM (List.map P.visible_object_position) $ P.visibleObjects isPlayer
           -- FIXME: what if there is more than one player
           player_position <- MaybeT $ return $ listToMaybe visible_player_locations
           (rand_x :: Integer) <- lift $ getRandomR (1,100)
           rand_face <- lift $ weightedPickM $ unweightedSet [minBound..maxBound]
           (_,my_position) <- lift P.whereAmI
           let face_to_player = faceAt my_position player_position
           return $ case distanceBetweenChessboard my_position player_position of
               _ | rand_x < 5 -> Wait -- if AI gets stuck, this will make sure they waste time so the game doesn't hang
               _ | rand_x < 20 -> FacingBehavior Step rand_face
               1 -> FacingBehavior Attack face_to_player
               -- x | x >= 10 -> Jump face_to_player  -- disable this until we can handle non-player teleporting sanely
               _ -> FacingBehavior Step face_to_player

