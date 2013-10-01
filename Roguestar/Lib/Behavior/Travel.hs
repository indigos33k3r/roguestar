{-# LANGUAGE ScopedTypeVariables #-}

module Roguestar.Lib.Behavior.Travel
    (stepMonster,
     turnMonster,
     ClimbOutcome,
     resolveClimb,
     executeClimb,
     TeleportJumpOutcome,
     resolveTeleportJump,
     executeTeleportJump,
     resolveStepWithHolographicTrail,
     resolveStepWithTemporalWeb)
    where

import Control.Monad.Maybe
import Roguestar.Lib.Data.FacingData
import Roguestar.Lib.DB as DB
import Roguestar.Lib.Core.Plane as Plane
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error
import Data.Ord
import Roguestar.Lib.Position as Position
import Roguestar.Lib.Data.TerrainData
import Data.List (minimumBy)
import Roguestar.Lib.Core.Monster
import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.Logging
import Roguestar.Lib.Data.TravelData
import Roguestar.Lib.Utility.DetailedLocation
import Roguestar.Lib.Utility.DetailedTravel as DetailedTravel
import Roguestar.Lib.Behavior.Outcome
import Roguestar.Lib.Time
import Roguestar.Lib.PlaneVisibility

data MoveOutcome =
    MoveGood { _move_monster :: MonsterRef, move_from :: Standing, _move_to :: Standing }
  | MoveFailedBecauseTerrainImpassable { move_from :: Standing }

walkMonster :: (DBReadable db) => Facing ->
                                   (Integer,Integer) ->
                                   MonsterRef ->
                                   db MoveOutcome
walkMonster face (x',y') monster_ref =
    do l <- DetailedTravel.whereIs monster_ref
       let (Parent plane_ref) = detail l
           Position (x,y) = detail l
           standing = Standing { standing_plane = plane_ref,
                                 standing_position = Position (x+x',y+y'),
                                 standing_facing = face }
       is_passable <- isTerrainPassable plane_ref
                                        monster_ref
                                        (standing_position standing)
       case () of
           () | not is_passable ->
               do logDB gameplay_log INFO $ "Terrain not passable."
                  return $ MoveFailedBecauseTerrainImpassable (detail l)
           () | otherwise ->
               return $ MoveGood monster_ref (detail l) standing

stepMonster :: (DBReadable db) => Facing -> MonsterRef -> db MoveOutcome
stepMonster face = walkMonster face (facingToRelative face)

turnMonster :: (DBReadable db) => Facing -> MonsterRef -> db MoveOutcome
turnMonster face = walkMonster face (0,0)

executeMoveMonster :: MoveOutcome -> DB ()
executeMoveMonster (MoveGood monster_ref _ to) =
    do _ <- move monster_ref to
       return ()
executeMoveMonster (MoveFailedBecauseTerrainImpassable {}) =
       return ()

instance Effect MoveOutcome where
    applyEffect = executeMoveMonster

instance Outcome MoveOutcome where
    failureMode (MoveFailedBecauseTerrainImpassable {}) = Abort
    failureMode _ = Success

instance HasDuration MoveOutcome where
    getDuration (MoveFailedBecauseTerrainImpassable {}) = return 0.0
    getDuration (MoveGood monster_ref from to) = moveActionTime 1.0 (standing_position from, standing_position to) monster_ref

--------------------------------------------------------------------------------
--      Travel between planes.
--------------------------------------------------------------------------------

data ClimbOutcome =
    ClimbGood ClimbDirection MonsterRef Standing
  | ClimbFailed

-- |
-- Climb up or down between Planes.
--
resolveClimb :: (DBReadable db) => MonsterRef ->
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
-- Find a random teleport landing position.
-- The teleport attempt can be automatically retried a number of times, and the most accurate attempt will be used.
-- If the retries are negative, the teleport will be made artificially innacurate.
--
randomTeleportLanding :: (DBReadable db) => Integer -> PlaneRef -> Position -> Position -> db Position
randomTeleportLanding retries plane_ref source_destination goal_destination =
    do landings <- replicateM (fromInteger $ max 1 retries) $ (pickRandomClearSite 3) 0 0 goal_destination (not . (`elem` impassable_terrains)) plane_ref
       return $ minimumBy (comparing $ \p -> Position.distanceBetweenSquared goal_destination p ^ 2 * Position.distanceBetweenSquared source_destination p) landings

data TeleportJumpOutcome =
    TeleportJumpGood MonsterRef Standing
  | TeleportJumpFailed

-- |
-- Teleport jump a creature about 5-7 units in the specified direction.
--
resolveTeleportJump :: (DBReadable db) => MonsterRef -> Facing -> db TeleportJumpOutcome
resolveTeleportJump creature_ref face = liftM (fromMaybe TeleportJumpFailed) $ runMaybeT $
    do start_location <- lift $ DetailedTravel.whereIs creature_ref
       jump_roll <- lift $ getMonsterAbilityScore JumpSkill creature_ref
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

--------------------------------------------------------------------------------
--      Setting Terrain
--------------------------------------------------------------------------------

data SetTerrainEffect = SetTerrainEffect {
    set_terrain_position :: Position,
    set_terrain_plane :: PlaneRef,
    set_terrain_type :: Terrain }

executeSetTerrain :: SetTerrainEffect -> DB ()
executeSetTerrain outcome = setTerrainAt (set_terrain_plane outcome)
                                         (set_terrain_position outcome)
                                         (set_terrain_type outcome)

instance Effect SetTerrainEffect where
    applyEffect o = executeSetTerrain o

--------------------------------------------------------------------------------
--      Slowing Monsters
--------------------------------------------------------------------------------

data SlowMonsterEffect = SlowMonsterEffect {
    slow_monster_ref :: MonsterRef,
    slow_monster_amount :: Rational }

executeSlowMonster :: SlowMonsterEffect -> DB ()
executeSlowMonster outcome = increaseTime (slow_monster_ref outcome) (slow_monster_amount outcome)

instance Effect SlowMonsterEffect where
    applyEffect = executeSlowMonster

--------------------------------------------------------------------------------
--      HolographicTrail
--------------------------------------------------------------------------------

resolveStepWithHolographicTrail :: (DBReadable db) => Facing -> MonsterRef -> db (OutcomeWithEffect MoveOutcome (MoveOutcome, Maybe SetTerrainEffect))
resolveStepWithHolographicTrail facing monster_ref =
    do when (not $ facing `elem` [North,South,East,West]) $
           throwError $ DBError "resolveStepWithHolographicTrail: only allowed in the four NSEW directions"
       move_outcome <- stepMonster facing monster_ref
       let (plane_ref :: PlaneRef, position :: Position) = (standing_plane $ move_from move_outcome, standing_position $ move_from move_outcome)
       old_terrain_type <- terrainAt plane_ref position
       return $ OutcomeWithEffect
            move_outcome
            (move_outcome,
             if old_terrain_type `elem` difficult_terrains then Nothing else Just $ SetTerrainEffect position plane_ref ForceField)
            (getDuration move_outcome)

--------------------------------------------------------------------------------
--      TemporalWeb
--------------------------------------------------------------------------------

resolveStepWithTemporalWeb :: (DBReadable db) => Facing -> MonsterRef -> db (OutcomeWithEffect MoveOutcome (MoveOutcome,[SlowMonsterEffect]))
resolveStepWithTemporalWeb facing monster_ref =
    do move_outcome <- stepMonster facing monster_ref
       let (plane_ref :: PlaneRef, position :: Position) = (standing_plane $ move_from move_outcome, standing_position $ move_from move_outcome)
       t <- getDuration move_outcome
       faction <- getMonsterFaction monster_ref
       (vobs :: [MonsterRef]) <- liftM (mapMaybe coerceReference) $ dbGetVisibleObjectsForFaction (const $ return True) faction plane_ref
       slows <- forM vobs $ \vob ->
           do (p :: Position) <- liftM detail $ getPlanarLocation monster_ref
              return $ SlowMonsterEffect vob (t / fromInteger (Position.distanceBetweenSquared p position))
       return $ OutcomeWithEffect
           move_outcome
           (move_outcome, slows)
           (getDuration move_outcome)

