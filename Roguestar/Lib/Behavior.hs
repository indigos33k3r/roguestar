{-# LANGUAGE ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}

-- Mechanics
module Roguestar.Lib.Behavior
    (Behavior(..),
     FacingBehavior(..),
     facingBehavior,
     isBehaviorAvailable,
     executeBehavior)
    where

import Prelude hiding (getContents)
import Roguestar.Lib.DB
import Roguestar.Lib.Logging
import Roguestar.Lib.Position
import Roguestar.Lib.Facing
import Data.Ratio
import Roguestar.Lib.Tool
import Control.Monad.Error
import Roguestar.Lib.Behavior.Combat
import Roguestar.Lib.Behavior.Activate
import Roguestar.Lib.Behavior.Travel
import Roguestar.Lib.TravelData
import Roguestar.Lib.Creature
import Roguestar.Lib.CreatureData
import Roguestar.Lib.Core.Plane
import Roguestar.Lib.PlaneVisibility
import Data.List
import Roguestar.Lib.TerrainData
import Roguestar.Lib.Behavior.Make
import Roguestar.Lib.Behavior.Construction
import Roguestar.Lib.Core.Building
import Roguestar.Lib.Reference
import Roguestar.Lib.DetailedLocation
import Roguestar.Lib.PlaneData

data FacingBehavior =
    Step
  | TurnInPlace
  | Jump
  | Fire
  | Attack
  | ClearTerrain
  | ActivateBuilding
     deriving (Show)

--
-- Every possible behavior that a creature might take, AI or Human.
--
data Behavior =
    FacingBehavior FacingBehavior Facing
  | StepDown
  | StepUp
  | Pickup ToolRef
  | Wield ToolRef
  | Unwield
  | Drop ToolRef
  | Wait
  | Vanish
  | Activate
  | Make PrepareMake
      deriving (Show)

-- | Decide which FacingBehavior is most appropriate for for a character's situation.
facingBehavior :: (DBReadable db) => CreatureRef -> Facing -> db FacingBehavior
facingBehavior creature_ref face =
    do ((Parent plane_ref,pos) :: (Parent Plane,Position)) <- liftM detail $ getPlanarLocation creature_ref
       let facing_pos = offsetPosition (facingToRelative face) pos
       t <- terrainAt plane_ref facing_pos
       who :: [CreatureRef] <- liftM asChildren $ whatIsOccupying plane_ref facing_pos
       what :: [BuildingRef] <- liftM asChildren $ whatIsOccupying plane_ref facing_pos
       result <- case t of
           _ | not (null who) -> return Attack
           _ | not (null what) -> return ActivateBuilding
           Forest -> return $ ClearTerrain
           RockFace -> return $ ClearTerrain
           _ -> return $ Step
       logDB gameplay_log INFO ("facingBehavior is: " ++ show result)
       return result

-- | Indicates whether or not it is allowed for the specified creature to conduct
-- the specified behavior.  A true result here does not guarantee that the action
-- will succeed.
isBehaviorAvailable :: (DBReadable db) => Behavior -> CreatureRef -> db Bool
isBehaviorAvailable (FacingBehavior Jump _) creature_ref =
    do ((Parent plane_ref,pos) :: (Parent Plane,Position)) <- liftM detail $ getPlanarLocation creature_ref
       the_terrain <- terrainAt plane_ref pos
       creature_has_teleport_ability <- getCreatureSpecial Teleportation creature_ref
       return $
           creature_has_teleport_ability ||
           the_terrain == RecreantFactory
isBehaviorAvailable _ _ = return True

executeBehavior :: Behavior -> CreatureRef -> DB ()
executeBehavior the_behavior the_creature =
    do logDB gameplay_log INFO ("Running behavior: behavior=" ++ show the_behavior ++ " creature=" ++ (show $ toUID the_creature))
       available <- isBehaviorAvailable the_behavior the_creature
       when (not available) $
           throwError $ DBError $ "Behavior is not available:" ++ show the_behavior
       dbBehave_ the_behavior the_creature

dbBehave_ :: Behavior -> CreatureRef -> DB ()
dbBehave_ (FacingBehavior Step face) creature_ref =
    do (move_from,move_to) <- move creature_ref =<< stepCreature face creature_ref
       dbAdvanceTime creature_ref =<< case () of
           () | (move_from == move_to) -> return 0
           () | face == Here -> actionTime creature_ref -- counts as turning in place
           () | face `elem` [North,South,East,West] -> move1ActionTime creature_ref
           () | otherwise -> move2ActionTime creature_ref

dbBehave_ StepDown creature_ref =
    do _ <- atomic executeClimb $ resolveClimb creature_ref ClimbDown
       -- FIXME: should be conditional
       dbAdvanceTime creature_ref =<< move2ActionTime creature_ref

dbBehave_ StepUp creature_ref =
    do _ <- atomic executeClimb $ resolveClimb creature_ref ClimbUp
       -- FIXME: should be conditional
       dbAdvanceTime creature_ref =<< move2ActionTime creature_ref

dbBehave_ (FacingBehavior Jump face) creature_ref =
    do _ <- atomic executeTeleportJump $ resolveTeleportJump creature_ref face
       dbAdvanceTime creature_ref =<< move2ActionTime creature_ref

dbBehave_ (FacingBehavior TurnInPlace face) creature_ref =
    do _ <- move creature_ref =<< turnCreature face creature_ref
       dbAdvanceTime creature_ref =<< actionTime creature_ref

dbBehave_ (Pickup tool_ref) creature_ref =
    do _ <- move tool_ref =<< pickupTool creature_ref tool_ref
       dbAdvanceTime creature_ref =<< actionTime creature_ref

dbBehave_ (Wield tool_ref) creature_ref =
    do available <- availableWields creature_ref
       already_wielded <- getWielded creature_ref
       when (not $ tool_ref `elem` available) $ throwError $ DBErrorFlag ToolIs_Unreachable
       _ <- move tool_ref =<< wieldTool tool_ref
       dbAdvanceTime creature_ref =<< case () of
           () | Just tool_ref == already_wielded -> return 0 -- already wielded, so this was an empty action
           () | otherwise -> actionTime creature_ref

dbBehave_ (Unwield) creature_ref =
    do dbUnwieldCreature creature_ref
       dbAdvanceTime creature_ref =<< actionTime creature_ref

dbBehave_ (Drop tool_ref) creature_ref =
    do tool_parent <- liftM parentReference $ whereIs tool_ref
       already_wielded <- getWielded creature_ref
       when (tool_parent =/= creature_ref) $ throwError $ DBErrorFlag ToolIs_NotInInventory
       _ <- move tool_ref =<< dropTool tool_ref
       dbAdvanceTime creature_ref =<< case () of
           () | Just tool_ref == already_wielded -> return 0  -- instantly drop a tool if it's already held in the hand
           () | otherwise -> actionTime creature_ref

dbBehave_ (FacingBehavior Fire face) creature_ref =
    do _ <- move creature_ref =<< turnCreature face creature_ref
       ranged_attack_model <- rangedAttackModel creature_ref
       _ <- atomic executeAttackChain $ resolveAttackChain ranged_attack_model (Left face)
       dbAdvanceTime creature_ref =<< actionTime creature_ref
       return ()

dbBehave_ (FacingBehavior Attack face) creature_ref =
    do _ <- move creature_ref =<< turnCreature face creature_ref
       melee_attack_model <- meleeAttackModel creature_ref
       _ <- atomic executeAttackChain $ resolveAttackChain melee_attack_model (Left face)
       dbAdvanceTime creature_ref =<< actionTime creature_ref
       return ()

dbBehave_ Wait creature_ref = dbAdvanceTime creature_ref =<< actionTime creature_ref

dbBehave_ Vanish creature_ref =
    do dbAdvanceTime creature_ref =<< actionTime creature_ref
       (Parent plane_ref :: Parent Plane) <- liftM detail $ getPlanarLocation creature_ref
       faction <- getCreatureFaction creature_ref
       is_visible_to_anyone_else <- liftM (any (genericReference creature_ref `elem`)) $
           mapM (\fact -> dbGetVisibleObjectsForFaction (return . const True) fact plane_ref)
                ({- all factions except this one: -} delete faction [minBound..maxBound])
       when (not is_visible_to_anyone_else) $ deleteCreature creature_ref
       return ()

dbBehave_ Activate creature_ref =
    do _ <- atomic executeActivation $ resolveActivation creature_ref
       dbAdvanceTime creature_ref =<< actionTime creature_ref
       return ()

dbBehave_ (Make make_prep) creature_ref =
    do _ <- atomic executeMake $ resolveMake creature_ref make_prep
       dbAdvanceTime creature_ref =<< actionTime creature_ref
       return ()

dbBehave_ (FacingBehavior ClearTerrain face) creature_ref =
    do _ <- move creature_ref =<< turnCreature face creature_ref
       ok <- modifyFacingTerrain clearTerrain face creature_ref
       when (not ok) $ throwError $ DBErrorFlag Unable
       dbAdvanceTime creature_ref =<< actionTime creature_ref
       return ()

dbBehave_ (FacingBehavior ActivateBuilding face) creature_ref =
    do _ <- move creature_ref =<< turnCreature face creature_ref
       ok <- activateFacingBuilding face creature_ref
       when (not ok) $ throwError $ DBErrorFlag Unable
       dbAdvanceTime creature_ref =<< actionTime creature_ref

{---------------------------------------------------------------------------------------------------
-- These are functions related to determing how long it takes for a creature to execute an action.
----------------------------------------------------------------------------------------------------}

getBaseSpeed :: (DBReadable db) => CreatureRef -> db Integer
getBaseSpeed creature_ref =
    do c <- dbGetCreature creature_ref
       let raw_speed = rawScore Speed c
       when (raw_speed <= 0) $ error $ "getBaseSpeed: Non-positive raw speed (" ++ show c ++ ")"
       return raw_speed

-- | Time required to do a simple physical task.
actionTime :: (DBReadable db) => CreatureRef -> db Rational
actionTime creature_ref =
    do raw_speed <- getBaseSpeed creature_ref
       return $ 1000 % (1000 + raw_speed)

-- | Time required to move one step.
move1ActionTime :: (DBReadable db) => CreatureRef -> db Rational
move1ActionTime creature_ref =
    do raw_speed <- getBaseSpeed creature_ref
       return $ 100 % (100+raw_speed)

-- | Time required to move diagonally one step.
move2ActionTime :: (DBReadable db) => CreatureRef -> db Rational
move2ActionTime = liftM (*1.4142) . move1ActionTime
