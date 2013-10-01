module Roguestar.Lib.Data.BehaviorData
    (FacingBehavior(..),
     Behavior(..))
    where

import Roguestar.Lib.Data.FacingData
import Roguestar.Lib.Data.LocationData
import Roguestar.Lib.Data.MakeData

data FacingBehavior =
    Step
  | TurnInPlace
  | Jump
  | Fire
  | Attack
  | ClearTerrain
  | ActivateBuilding
  | TemporalWebStep
  | HolographicTrailStep
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


