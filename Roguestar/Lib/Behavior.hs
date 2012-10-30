{-# LANGUAGE ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}

-- Mechanics
module Roguestar.Lib.Behavior
    (Behavior(..),
     facingBehavior,
     dbBehave)
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
import Roguestar.Lib.Activate
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
import Roguestar.Lib.Building
import Roguestar.Lib.Reference
import Roguestar.Lib.DetailedLocation
import Roguestar.Lib.PlaneData

--
-- Every possible behavior that a creature might take, AI or Human.
--
data Behavior =
    Step Facing
  | TurnInPlace Facing
  | StepDown
  | StepUp
  | Jump Facing
  | Pickup ToolRef
  | Wield ToolRef
  | Unwield
  | Drop ToolRef
  | Fire Facing
  | Attack Facing
  | Wait
  | Vanish
  | Activate
  | Make PrepareMake
  | ClearTerrain Facing
  | ActivateBuilding Facing
        deriving (Show)

-- | Get an appropriate behavior facing in the given direction.
-- If the adjacent facing square is empty, this is 'Step', but
-- if occupied by a creature this is 'Attack'.
facingBehavior :: (DBReadable db) => CreatureRef -> Facing -> db Behavior
facingBehavior creature_ref face =
    do ((Parent plane_ref,pos) :: (Parent Plane,Position)) <- liftM detail $ getPlanarLocation creature_ref
       let facing_pos = offsetPosition (facingToRelative face) pos
       t <- terrainAt plane_ref facing_pos
       who :: [CreatureRef] <- liftM asChildren $ whatIsOccupying plane_ref facing_pos
       what :: [BuildingRef] <- liftM asChildren $ whatIsOccupying plane_ref facing_pos
       result <- case t of
           _ | not (null who) -> return $ Attack face
           _ | not (null what) -> return $ ActivateBuilding face
           Forest -> return $ ClearTerrain face
           RockFace -> return $ ClearTerrain face
           _ -> return $ Step face
       logDB log_behavior INFO ("facingBehavior is: " ++ show result)
       return result

dbBehave :: Behavior -> CreatureRef -> DB ()
dbBehave the_behavior the_creature =
    do logDB log_behavior INFO ("Running behavior: behavior=" ++ show the_behavior ++ " creature=" ++ (show $ toUID the_creature))
       dbBehave_ the_behavior the_creature

dbBehave_ :: Behavior -> CreatureRef -> DB ()
dbBehave_ (Step face) creature_ref =
    do (move_from,move_to) <- move creature_ref =<< stepCreature face creature_ref
       dbAdvanceTime creature_ref =<< case () of
           () | (move_from == move_to) -> return 0
           () | face == Here -> quickActionTime creature_ref -- counts as turning in place
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

dbBehave_ (Jump face) creature_ref =
    do _ <- atomic executeTeleportJump $ resolveTeleportJump creature_ref face
       dbAdvanceTime creature_ref =<< move2ActionTime creature_ref

dbBehave_ (TurnInPlace face) creature_ref =
    do _ <- move creature_ref =<< turnCreature face creature_ref
       dbAdvanceTime creature_ref =<< quickActionTime creature_ref

dbBehave_ (Pickup tool_ref) creature_ref =
    do _ <- move tool_ref =<< pickupTool creature_ref tool_ref
       dbAdvanceTime creature_ref =<< quickActionTime creature_ref

dbBehave_ (Wield tool_ref) creature_ref =
    do available <- availableWields creature_ref
       already_wielded <- getWielded creature_ref
       when (not $ tool_ref `elem` available) $ throwError $ DBErrorFlag ToolIs_Unreachable
       _ <- move tool_ref =<< wieldTool tool_ref
       dbAdvanceTime creature_ref =<< case () of
           () | Just tool_ref == already_wielded -> return 0 -- already wielded, so this was an empty action
           () | otherwise -> quickActionTime creature_ref

dbBehave_ (Unwield) creature_ref =
    do dbUnwieldCreature creature_ref
       dbAdvanceTime creature_ref =<< quickActionTime creature_ref

dbBehave_ (Drop tool_ref) creature_ref =
    do tool_parent <- liftM parentReference $ whereIs tool_ref
       already_wielded <- getWielded creature_ref
       when (tool_parent =/= creature_ref) $ throwError $ DBErrorFlag ToolIs_NotInInventory
       _ <- move tool_ref =<< dropTool tool_ref
       dbAdvanceTime creature_ref =<< case () of
           () | Just tool_ref == already_wielded -> return 0  -- instantly drop a tool if it's already held in the hand
           () | otherwise -> quickActionTime creature_ref

dbBehave_ (Fire face) creature_ref =
    do _ <- move creature_ref =<< turnCreature face creature_ref
       ranged_attack_model <- rangedAttackModel creature_ref
       _ <- atomic executeAttack $ resolveAttack ranged_attack_model face
       dbAdvanceTime creature_ref =<< quickActionTime creature_ref
       return ()

dbBehave_ (Attack face) creature_ref =
    do _ <- move creature_ref =<< turnCreature face creature_ref
       melee_attack_model <- meleeAttackModel creature_ref
       _ <- atomic executeAttack $ resolveAttack melee_attack_model face
       dbAdvanceTime creature_ref =<< quickActionTime creature_ref
       return ()

dbBehave_ Wait creature_ref = dbAdvanceTime creature_ref =<< quickActionTime creature_ref

dbBehave_ Vanish creature_ref =
    do dbAdvanceTime creature_ref =<< quickActionTime creature_ref
       (Parent plane_ref :: Parent Plane) <- liftM detail $ getPlanarLocation creature_ref
       faction <- getCreatureFaction creature_ref
       is_visible_to_anyone_else <- liftM (any (genericReference creature_ref `elem`)) $
           mapM (\fact -> dbGetVisibleObjectsForFaction (return . const True) fact plane_ref)
                ({- all factions except this one: -} delete faction [minBound..maxBound])
       when (not is_visible_to_anyone_else) $ deleteCreature creature_ref
       return ()

dbBehave_ Activate creature_ref =
    do _ <- atomic executeActivation $ resolveActivation creature_ref
       dbAdvanceTime creature_ref =<< quickActionTime creature_ref
       return ()

dbBehave_ (Make make_prep) creature_ref =
    do _ <- atomic executeMake $ resolveMake creature_ref make_prep
       dbAdvanceTime creature_ref =<< quickActionTime creature_ref
       return ()

dbBehave_ (ClearTerrain face) creature_ref =
    do _ <- move creature_ref =<< turnCreature face creature_ref
       ok <- modifyFacingTerrain clearTerrain face creature_ref
       when (not ok) $ throwError $ DBErrorFlag Unable
       dbAdvanceTime creature_ref =<< quickActionTime creature_ref
       return ()

dbBehave_ (ActivateBuilding face) creature_ref =
    do _ <- move creature_ref =<< turnCreature face creature_ref
       ok <- activateFacingBuilding face creature_ref
       when (not ok) $ throwError $ DBErrorFlag Unable
       dbAdvanceTime creature_ref =<< quickActionTime creature_ref

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
quickActionTime :: (DBReadable db) => CreatureRef -> db Rational
quickActionTime creature_ref =
    do raw_speed <- getBaseSpeed creature_ref
       return $ 50 % (100 + raw_speed `div` 2)

-- | Time required to move one step.
move1ActionTime :: (DBReadable db) => CreatureRef -> db Rational
move1ActionTime creature_ref =
    do raw_speed <- getBaseSpeed creature_ref
       return $ 100 % (100+raw_speed)

-- | Time required to move diagonally one step.
move2ActionTime :: (DBReadable db) => CreatureRef -> db Rational
move2ActionTime = liftM (*1.4142) . move1ActionTime
