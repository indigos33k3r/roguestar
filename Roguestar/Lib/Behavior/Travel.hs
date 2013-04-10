{-# LANGUAGE ScopedTypeVariables #-}

module Roguestar.Lib.Behavior.Travel
    (stepCreature,
     turnCreature,
     ClimbOutcome,
     resolveClimb,
     executeClimb,
     TeleportJumpOutcome,
     resolveTeleportJump,
     executeTeleportJump)
    where

import Control.Monad.Maybe
import Roguestar.Lib.Facing
import Roguestar.Lib.DB as DB
import Roguestar.Lib.Core.Plane
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Data.Ord
import Roguestar.Lib.Position as Position
import Roguestar.Lib.TerrainData
import Data.List (minimumBy)
import Roguestar.Lib.Core.Monster
import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.Logging
import Roguestar.Lib.TravelData
import Roguestar.Lib.DetailedLocation
import Roguestar.Lib.DetailedTravel as DetailedTravel

walkCreature :: (DBReadable db) => Facing ->
                                   (Integer,Integer) ->
                                   CreatureRef ->
                                   db Standing
walkCreature face (x',y') creature_ref =
    do l <- DetailedTravel.whereIs creature_ref
       let (Parent plane_ref) = detail l
           Position (x,y) = detail l
           standing = Standing { standing_plane = plane_ref,
                                 standing_position = Position (x+x',y+y'),
                                 standing_facing = face }
       is_passable <- isTerrainPassable plane_ref
                                        creature_ref
                                        (standing_position standing)
       case () of
           () | not is_passable ->
               do logDB gameplay_log INFO $ "Terrain not passable."
                  return $ detail l
           () | otherwise ->
               return $ standing

stepCreature :: (DBReadable db) => Facing -> CreatureRef -> db Standing
stepCreature face = walkCreature face (facingToRelative face)

turnCreature :: (DBReadable db) => Facing -> CreatureRef -> db Standing
turnCreature face = walkCreature face (0,0)

--------------------------------------------------------------------------------
--      Travel between planes.
--------------------------------------------------------------------------------

data ClimbOutcome =
    ClimbGood ClimbDirection CreatureRef Standing
  | ClimbFailed

-- |
-- Climb up or down between Planes.
--
resolveClimb :: (DBReadable db) => CreatureRef ->
                                   ClimbDirection ->
                                   db ClimbOutcome
resolveClimb creature_ref direction = liftM (fromMaybe ClimbFailed) $ runMaybeT $
    do l <- lift $ DetailedTravel.whereIs creature_ref
       let plane_ref :: PlaneRef = asParent $ detail l
           pos :: Position = detail l
           face :: Facing = detail l
       terrain_type <- lift $ terrainAt plane_ref pos
       let (expected_starting_terrain, expected_landing_terrain) = case direction of
               ClimbUp -> (Upstairs,Downstairs)
               ClimbDown -> (Downstairs,Upstairs)
       when (terrain_type /= expected_starting_terrain) $
           do lift $ logDB gameplay_log WARNING $ "Not standing on correct stairway."
              MaybeT $ return Nothing
       lift $ logDB gameplay_log DEBUG $ "Stepping " ++ show direction ++ " from: " ++ show (plane_ref,pos)
       plane_destination <- MaybeT $ case direction of
                 ClimbDown -> getBeneath plane_ref
                 ClimbUp -> liftM (fmap asParent . fromLocation) $ DB.whereIs plane_ref
       lift $ logDB gameplay_log DEBUG $ "Stepping " ++ show direction ++ " to: " ++ show plane_destination
       pos' <- lift $ pickRandomClearSite 10 0 0 pos (== expected_landing_terrain) plane_destination
       return $ ClimbGood direction creature_ref $
           Standing { standing_plane = plane_destination,
                      standing_position = pos',
                      standing_facing = face }

-- | Execute a resolved climb attempt.
executeClimb :: ClimbOutcome -> DB ()
executeClimb ClimbFailed = return ()
executeClimb (ClimbGood direction creature_ref standing_location) =
    do _ <- move creature_ref standing_location
       dbPushSnapshot $ ClimbEvent direction creature_ref
       return ()

--------------------------------------------------------------------------------
--      Teleportation/Jumping
--------------------------------------------------------------------------------

-- |
-- Try to teleport the creature to the specified Position.  The teleport attempt can be automatically retried a number of times, and the most accurate attempt will be used.
-- If the retries are negative, the teleport will be made artificially innacurate.
--
randomTeleportLanding :: (DBReadable db) => Integer -> PlaneRef -> Position -> Position -> db Position
randomTeleportLanding retries plane_ref source_destination goal_destination =
    do landings <- replicateM (fromInteger $ max 1 retries) $ (pickRandomClearSite 3) 0 0 goal_destination (not . (`elem` impassable_terrains)) plane_ref
       return $ minimumBy (comparing $ \p -> Position.distanceBetweenSquared goal_destination p ^ 2 * Position.distanceBetweenSquared source_destination p) landings

data TeleportJumpOutcome =
    TeleportJumpGood CreatureRef Standing
  | TeleportJumpFailed

-- |
-- Teleport jump a creature about 5-7 units in the specified direction.
--
resolveTeleportJump :: (DBReadable db) => CreatureRef -> Facing -> db TeleportJumpOutcome
resolveTeleportJump creature_ref face = liftM (fromMaybe TeleportJumpFailed) $ runMaybeT $
    do start_location <- lift $ DetailedTravel.whereIs creature_ref
       jump_roll <- lift $ getCreatureAbilityScore JumpSkill creature_ref
       landing_position <- lift $ randomTeleportLanding (jump_roll `div` 5 + 1)
                                                        (asParent $ detail start_location)
                                                        (detail start_location) $
                                                        offsetPosition (facingToRelative7 face) $ detail start_location
       let good = TeleportJumpGood creature_ref $ Standing { standing_plane = asParent (detail start_location),
                                                             standing_position = landing_position,
                                                             standing_facing = face }
           bad = TeleportJumpFailed
       lift $ weightedPickM $ weightedSet [(jump_roll,good),
                                           (1,bad)]

-- | Execute a resolved teleport jump.
executeTeleportJump :: TeleportJumpOutcome -> DB ()
executeTeleportJump TeleportJumpFailed = return ()
executeTeleportJump (TeleportJumpGood creature_ref standing_location) =
    do _ <- move creature_ref standing_location
       dbPushSnapshot $ TeleportEvent creature_ref
       return ()

