{-# LANGUAGE ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}

-- Mechanics
module Roguestar.Lib.Behavior
    (facingBehavior,
     isBehaviorAvailable,
     executeBehavior)
    where

import Prelude hiding (getContents)
import Roguestar.Lib.DB
import Roguestar.Lib.Logging
import Roguestar.Lib.Position
import Roguestar.Lib.Data.FacingData
import Roguestar.Lib.Time
import Roguestar.Lib.Tool
import Control.Monad.Error
import Control.Monad.Reader
import Roguestar.Lib.Behavior.Combat
import Roguestar.Lib.Behavior.Activate
import Roguestar.Lib.Behavior.Travel
import Roguestar.Lib.Behavior.Outcome
import Roguestar.Lib.Data.TravelData
import Roguestar.Lib.Core.Monster
import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.Core.Plane
import Roguestar.Lib.PlaneVisibility
import Data.List
import Roguestar.Lib.Data.TerrainData
import Roguestar.Lib.Behavior.Make
import Roguestar.Lib.Behavior.Construction
import Roguestar.Lib.Core.Building
import Roguestar.Lib.Utility.DetailedLocation
import Roguestar.Lib.Data.PlaneData
import Roguestar.Lib.Data.BehaviorData

-- | Decide which FacingBehavior is most appropriate for for a character's situation.
facingBehavior :: (DBReadable db) => MonsterRef -> Facing -> db FacingBehavior
facingBehavior creature_ref face =
    do ((Parent plane_ref,pos) :: (Parent PlaneData,Position)) <- liftM detail $ getPlanarLocation creature_ref
       let facing_pos = offsetPosition (facingToRelative face) pos
       t <- terrainAt plane_ref facing_pos
       who :: [MonsterRef] <- liftM asChildren $ whatIsOccupying plane_ref facing_pos
       what :: [BuildingRef] <- liftM asChildren $ whatIsOccupying plane_ref facing_pos
       result <- case t of
           _ | not (null who) -> return Attack
           _ | not (null what) -> return ActivateBuilding
           _ | t `elem` impassable_terrains -> return $ ClearTerrain
           _ -> return $ Step
       logDB gameplay_log INFO ("facingBehavior is: " ++ show result)
       return result

-- | Indicates whether or not it is allowed for the specified creature to conduct
-- the specified behavior.  A true result here does not guarantee that the action
-- will succeed.
isBehaviorAvailable :: (DBReadable db) => Behavior -> MonsterRef -> db Bool
isBehaviorAvailable (FacingBehavior Jump _) creature_ref =
    do ((Parent plane_ref,pos) :: (Parent PlaneData,Position)) <- liftM detail $ getPlanarLocation creature_ref
       the_terrain <- terrainAt plane_ref pos
       creature_has_teleport_ability <- getMonsterSpecial Teleportation creature_ref
       return $
           creature_has_teleport_ability ||
           the_terrain == RecreantFactory
isBehaviorAvailable (FacingBehavior TemporalWebStep _) creature_ref = getMonsterSpecial TemporalWeb creature_ref
isBehaviorAvailable (FacingBehavior HolographicTrailStep facing) creature_ref =
    do has_special <- getMonsterSpecial HolographicTrail creature_ref
       return $ has_special && facing `elem` [North,South,East,West,Here]
isBehaviorAvailable _ _ = return True

executeBehavior :: Behavior -> MonsterRef -> DB ()
executeBehavior the_behavior the_creature =
    do logDB gameplay_log INFO ("Running behavior: behavior=" ++ show the_behavior ++ " creature=" ++ (show $ toUID the_creature))
       available <- isBehaviorAvailable the_behavior the_creature
       when (not available) $
           throwError $ DBError $ "Behavior is not available:" ++ show the_behavior
       dbBehave_ the_behavior the_creature

dbBehave_ :: Behavior -> MonsterRef -> DB ()
dbBehave_ (FacingBehavior Step face) creature_ref =
    do motion_outcome <- stepMonster face creature_ref
       applyEffect motion_outcome
       increaseTime creature_ref =<< getDuration motion_outcome

dbBehave_ (FacingBehavior TemporalWebStep face) creature_ref =
    do motion_outcome <- resolveStepWithTemporalWeb face creature_ref
       applyEffect motion_outcome
       increaseTime creature_ref =<< getDuration motion_outcome

dbBehave_ (FacingBehavior HolographicTrailStep face) creature_ref =
    do holo_outcome <- resolveStepWithHolographicTrail face creature_ref
       applyEffect holo_outcome
       increaseTime creature_ref =<< getDuration holo_outcome

dbBehave_ StepDown creature_ref =
    do _ <- executeClimb =<< resolveClimb creature_ref ClimbDown
       -- FIXME: should be conditional
       increaseTime creature_ref =<< actionTime creature_ref

dbBehave_ StepUp creature_ref =
    do _ <- executeClimb =<< resolveClimb creature_ref ClimbUp
       -- FIXME: should be conditional
       increaseTime creature_ref =<< actionTime creature_ref

dbBehave_ (FacingBehavior Jump face) creature_ref =
    do _ <- executeTeleportJump =<< resolveTeleportJump creature_ref face
       increaseTime creature_ref =<< actionTime creature_ref -- FIXME: this should use moveActionTime

dbBehave_ (FacingBehavior TurnInPlace face) monster_ref =
    do turn_outcome <- turnMonster face monster_ref
       applyEffect turn_outcome
       increaseTime monster_ref =<< getDuration turn_outcome

dbBehave_ (Pickup tool_ref) creature_ref =
    do _ <- move tool_ref =<< pickupTool creature_ref tool_ref
       increaseTime creature_ref =<< actionTime creature_ref

dbBehave_ (Wield tool_ref) creature_ref =
    do available <- availableWields creature_ref
       already_wielded <- getWielded creature_ref
       when (not $ tool_ref `elem` available) $ throwError $ DBErrorFlag ToolIs_Unreachable
       _ <- move tool_ref =<< wieldTool tool_ref
       increaseTime creature_ref =<< case () of
           () | Just tool_ref == already_wielded -> return 0 -- already wielded, so this was an empty action
           () | otherwise -> actionTime creature_ref

dbBehave_ (Unwield) creature_ref =
    do dbUnwieldMonster creature_ref
       increaseTime creature_ref =<< actionTime creature_ref

dbBehave_ (Drop tool_ref) creature_ref =
    do tool_parent <- liftM parentReference $ asks $ whereIs tool_ref
       already_wielded <- getWielded creature_ref
       when (tool_parent =/= creature_ref) $ throwError $ DBErrorFlag ToolIs_NotInInventory
       _ <- move tool_ref =<< dropTool tool_ref
       increaseTime creature_ref =<< case () of
           () | Just tool_ref == already_wielded -> return 0  -- instantly drop a tool if it's already held in the hand
           () | otherwise -> actionTime creature_ref

dbBehave_ (FacingBehavior Fire face) creature_ref =
    do turn_outcome <- turnMonster face creature_ref
       applyEffect turn_outcome
       ranged_attack_model <- rangedAttackModel creature_ref
       _ <- executeAttackChain =<< resolveAttackChain ranged_attack_model (Left face)
       increaseTime creature_ref =<< actionTime creature_ref
       return ()

dbBehave_ (FacingBehavior Attack face) creature_ref =
    do turn_outcome <- turnMonster face creature_ref
       applyEffect turn_outcome
       melee_attack_model <- meleeAttackModel creature_ref
       _ <- executeAttackChain =<< resolveAttackChain melee_attack_model (Left face)
       increaseTime creature_ref =<< actionTime creature_ref
       return ()

dbBehave_ Wait creature_ref = increaseTime creature_ref =<< actionTime creature_ref

dbBehave_ Vanish creature_ref =
    do increaseTime creature_ref =<< actionTime creature_ref
       (Parent plane_ref :: Parent PlaneData) <- liftM detail $ getPlanarLocation creature_ref
       faction <- getMonsterFaction creature_ref
       is_visible_to_anyone_else <- liftM (any (genericReference creature_ref `elem`)) $
           mapM (\fact -> dbGetVisibleObjectsForFaction (return . const True) fact plane_ref)
                ({- all factions except this one: -} delete faction [minBound..maxBound])
       when (not is_visible_to_anyone_else) $ deleteMonster creature_ref
       return ()

dbBehave_ Activate creature_ref =
    do _ <- executeActivation =<< resolveActivation creature_ref
       increaseTime creature_ref =<< actionTime creature_ref
       return ()

dbBehave_ (Make make_prep) creature_ref =
    do _ <- atomic executeMake $ resolveMake creature_ref make_prep
       increaseTime creature_ref =<< actionTime creature_ref
       return ()

dbBehave_ (FacingBehavior ClearTerrain face) creature_ref =
    do turn_outcome <- turnMonster face creature_ref
       applyEffect turn_outcome
       ok <- modifyFacingTerrain clearTerrain face creature_ref
       when (not ok) $ throwError $ DBErrorFlag Unable
       increaseTime creature_ref =<< actionTime creature_ref
       return ()

dbBehave_ (FacingBehavior ActivateBuilding face) creature_ref =
    do turn_outcome <- turnMonster face creature_ref
       applyEffect turn_outcome
       ok <- activateFacingBuilding face creature_ref
       when (not ok) $ throwError $ DBErrorFlag Unable
       increaseTime creature_ref =<< actionTime creature_ref


