{-# LANGUAGE PatternGuards, PatternSignatures #-}

module Turns
    (dbPerformPlayerTurn)
    where

import Control.Monad.Maybe
import Control.Monad.Trans
import DB
import DBData
import FactionData
import Races
import Plane
import Control.Monad
import Creature
import Data.Ratio
import Facing
import Dice
import TerrainData
import Data.Maybe
import Behavior
import qualified Perception as P
import Position

dbPerformPlayerTurn :: Behavior -> CreatureRef -> DB ()
dbPerformPlayerTurn beh creature_ref =
    do dbBehave beh creature_ref
       dbFinishPendingAITurns

dbFinishPendingAITurns :: DB ()
dbFinishPendingAITurns =
    do m_current_plane <- dbGetCurrentPlane
       case m_current_plane of
           Just p -> dbFinishPlanarAITurns p
	   Nothing -> return ()

dbFinishPlanarAITurns :: PlaneRef -> DB ()
dbFinishPlanarAITurns plane_ref =
    do (all_creatures_on_plane :: [CreatureRef]) <- dbGetContents plane_ref
       any_players_left <- liftM (any (== Player)) $ mapM getCreatureFaction all_creatures_on_plane
       next_turn <- dbNextTurn $ map generalizeReference all_creatures_on_plane ++ [generalizeReference plane_ref]
       case next_turn of
           _ | not any_players_left ->
	       do setPlayerState GameOver
	          return ()
	   ref | ref =:= plane_ref -> 
	       do dbPerform1PlanarAITurn plane_ref
	          dbFinishPlanarAITurns plane_ref
	   ref | Just creature_ref <- coerceReferenceTyped _creature ref -> 
	       do faction <- getCreatureFaction creature_ref
	          if (faction /= Player)
		      then do dbPerform1CreatureAITurn creature_ref
		              dbFinishPlanarAITurns plane_ref
		      else setPlayerState (PlayerCreatureTurn creature_ref NormalMode)
		  return ()
	   _ -> error "dbFinishPlanarAITurns: impossible case"

dbPerform1PlanarAITurn :: PlaneRef -> DB ()
dbPerform1PlanarAITurn plane_ref = 
    do creature_locations <- dbGetContents plane_ref
       player_locations <- filterRO (liftM (== Player) . getCreatureFaction . entity) creature_locations
       native_locations <- filterRO (liftM (/= Player) . getCreatureFaction . entity) creature_locations
       when (length native_locations < length player_locations * 2) $
           do p <- roll $ map location player_locations
	      spawn_position <- pickRandomClearSite 5 0 0 p (== RecreantFactory) plane_ref
	      dbNewCreature Pirates recreant (Standing plane_ref spawn_position Here)
	      return ()
       dbAdvanceTime (1%100) plane_ref

dbPerform1CreatureAITurn :: CreatureRef -> DB ()
dbPerform1CreatureAITurn creature_ref = 
    atomic $ liftM (flip dbBehave creature_ref) $ P.runPerception creature_ref $ liftM (fromMaybe Vanish) $ runMaybeT $
        do player <- MaybeT $ liftM listToMaybe $ filterM (liftM (== Player) . P.getCreatureFaction . entity) =<< P.visibleObjects 
           my_position <- lift P.myPosition
	   let face_to_player = faceAt my_position (location player)
	   return $ case distanceBetweenChessboard my_position (location player) of
	       1 -> Attack $ face_to_player
	       _ -> Step $ face_to_player
