{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

module Roguestar.Lib.Turns
    (dbPerformPlayerTurn)
    where

import Prelude hiding (getContents)
import Control.Monad.Maybe
import Control.Monad.Trans
import Roguestar.Lib.DB
import Roguestar.Lib.Reference
import Roguestar.Lib.FactionData
import Roguestar.Lib.SpeciesData
import Roguestar.Lib.CreatureData (Creature)
import Roguestar.Lib.Plane
import Control.Monad
import Roguestar.Lib.Creature
import Data.Ratio
import Roguestar.Lib.Facing
import Roguestar.Lib.TerrainData
import Data.Maybe
import Roguestar.Lib.Behavior
import qualified Roguestar.Lib.Perception as P
import Roguestar.Lib.Position
import Roguestar.Lib.PlayerState
import Roguestar.Lib.Logging
import Roguestar.Lib.DetailedLocation
import Control.Monad.Random

dbPerformPlayerTurn :: Behavior -> CreatureRef -> DB ()
dbPerformPlayerTurn beh creature_ref =
    do logDB log_turns INFO $ "dbPerformPlayerTurn; Beginning player action: " ++ show beh
       dbBehave beh creature_ref
       logDB log_turns INFO $ "dbPerformPlayerTurn; Doing AI turns:"
       dbFinishPendingAITurns
       logDB log_turns INFO $ "dbPerformPlayerTurn; Finished all player and AI turns."

dbFinishPendingAITurns :: DB ()
dbFinishPendingAITurns =
    do m_current_plane <- getCurrentPlane
       case m_current_plane of
           Just p -> dbFinishPlanarAITurns p
           Nothing -> return ()

dbFinishPlanarAITurns :: PlaneRef -> DB ()
dbFinishPlanarAITurns plane_ref =
    do logDB log_turns INFO $ "Running turns for plane: id=" ++ show (toUID plane_ref)
       sweepDead plane_ref
       (all_creatures_on_plane :: [CreatureRef]) <- liftM asChildren $ getContents plane_ref
       any_players_left <- liftM (any (== Player)) $ mapM getCreatureFaction all_creatures_on_plane
       next_turn <- dbNextTurn $ map genericReference all_creatures_on_plane ++ [genericReference plane_ref]
       case next_turn of
           _ | not any_players_left ->
               do logDB log_turns INFO $ "dbFinishPlanarAITurns; Game over condition detected"
                  setPlayerState $ GameOver PlayerIsDead
                  return ()
           ref | ref =:= plane_ref ->
               do dbPerform1PlanarAITurn plane_ref
                  dbFinishPlanarAITurns plane_ref
           ref | Just creature_ref <- coerceReference ref ->
               do faction <- getCreatureFaction creature_ref
                  if (faction /= Player)
                      then do dbPerform1CreatureAITurn creature_ref
                              dbFinishPlanarAITurns plane_ref
                      else setPlayerState (PlayerCreatureTurn creature_ref)
                  return ()
           _ -> error "dbFinishPlanarAITurns: impossible case"

planar_turn_frequency :: Integer
planar_turn_frequency = 100

monster_spawns :: [(TerrainPatch,Species)]
monster_spawns = [(RecreantFactory,RedRecreant)]

dbPerform1PlanarAITurn :: PlaneRef -> DB ()
dbPerform1PlanarAITurn plane_ref =
    do logDB log_turns INFO $ "dbPerform1PlanarAITurn; Beginning planar AI turn (for the plane itself):"
       (creature_locations :: [DetailedLocation (Child Creature)]) <- liftM mapLocations $ getContents plane_ref
       player_locations <- filterRO (liftM (== Player) . getCreatureFaction . asChild . detail) creature_locations
       num_npcs <- liftM length $ filterRO (liftM (/= Player) . getCreatureFaction . asChild . detail) creature_locations
       when (num_npcs < length player_locations * 3) $
           do (terrain_type,species) <- pickM monster_spawns
              _ <- spawnNPC terrain_type species plane_ref $ map detail $ player_locations
              return ()
       dbAdvanceTime plane_ref (1%planar_turn_frequency)

-- |
-- Spawn a non-player creature on the specified terrain type (or fail if not finding that terrain type)
-- and of the specified species, on the specified plane, near one of the specified positions
-- (presumably the list of positions of all player characters).
spawnNPC :: TerrainPatch -> Species -> PlaneRef -> [Position] -> DB Bool
spawnNPC terrain_type species plane_ref player_locations =
    do logDB log_turns INFO $ "spawnNPC; Spawning an NPC"
       p <- pickM player_locations
       m_spawn_position <- pickRandomClearSite_withTimeout (Just 2) 7 0 0 p (== terrain_type) plane_ref
       case m_spawn_position of
           Nothing -> return False
           Just spawn_position ->
               do _ <- newCreature Monsters species (Standing plane_ref spawn_position Here)
                  return True

dbPerform1CreatureAITurn :: CreatureRef -> DB ()
dbPerform1CreatureAITurn creature_ref =
    do logDB log_turns INFO $ "dbPerform1CreatureAITurn; Performing a creature's AI turn: id=" ++ show (toUID creature_ref)
       liftM (const ()) $ atomic (flip dbBehave creature_ref) $ P.runPerception creature_ref $ liftM (fromMaybe Vanish) $ runMaybeT $
        do let isPlayer :: forall db. (DBReadable db) => Reference () -> P.DBPerception db Bool
               isPlayer ref | (Just creature_ref) <- coerceReference ref =
                   do f <- P.getCreatureFaction creature_ref
                      return $ f == Player
               isPlayer _ | otherwise = return False
           (visible_player_locations :: [Position]) <- lift $ liftM (map P.visible_object_position) $ P.visibleObjects isPlayer
           -- FIXME: what if there is more than one player
           player_position <- MaybeT $ return $ listToMaybe visible_player_locations
           (rand_x :: Integer) <- lift $ getRandomR (1,100)
           rand_face <- lift $ pickM [minBound..maxBound]
           (_,my_position) <- lift P.whereAmI
           let face_to_player = faceAt my_position player_position
           return $ case distanceBetweenChessboard my_position player_position of
               _ | rand_x < 5 -> Wait -- if AI gets stuck, this will make sure they waste time so the game doesn't hang
               _ | rand_x < 20 -> Step rand_face
               1 -> Attack face_to_player
               -- x | x >= 10 -> Jump face_to_player  -- disable this until we can handle non-player teleporting sanely
               _ -> Step face_to_player

