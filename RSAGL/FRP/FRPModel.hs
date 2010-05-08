{-# LANGUAGE TypeFamilies, EmptyDataDecls, UndecidableInstances, FlexibleContexts #-}

-- | A model of the types used by an FRP program.
module RSAGL.FRP.FRPModel
    (Enabled, Disabled, FRPModel(..), FRP1, FRPX, FRPContext, FRP1Context, Switch, SimpleSwitch)
    where

import RSAGL.Auxiliary.RecombinantState

data Enabled
data Disabled

class RSAGL_FRP_FRPMODEL_ENABLED_DISABLED a where

instance RSAGL_FRP_FRPMODEL_ENABLED_DISABLED Enabled where
instance RSAGL_FRP_FRPMODEL_ENABLED_DISABLED Disabled where

class RSAGL_FRP_FRPMODEL a where

class (RSAGL_FRP_FRPMODEL frp,Eq (ThreadIDOf frp)) => FRPModel frp where
    -- | The threading capability, either 'Enabled' or 'Disabled'.
    type ThreadingOf frp :: *
    -- | The a type of the thread ID (for example, a unique integer).
    type ThreadIDOf frp :: *
    -- | The 'ArrowState' type.
    type StateOf frp :: *
    -- | The type of the switch input (used in switchTerminate/switchContinue, etc)
    type SwitchInputOf frp :: *
    -- | The type of the switch output (used in switchTerminate/switchContinue, etc)
    type SwitchOutputOf frp :: *
    -- | Unwrap to get the nested Switch type.
    type Unwrap frp :: *

instance RSAGL_FRP_FRPMODEL () where

instance FRPModel       () where
    type ThreadingOf    () = Disabled
    type ThreadIDOf     () = ()
    type StateOf        () = ()
    type SwitchInputOf  () = ()
    type SwitchOutputOf () = ()
    type Unwrap         () = ()

-- | The FRPModel type that represents a switch.  Consists of the following type variables.
--
-- Note: Don't pattern-match against this type directly, as it is a volatile interface.
-- Either use a type synonym, such as 'SimpleSwitch', or match against the type functions in FRPModel.
--
-- * k - See, ThreadingOf.
-- * t - See, ThreadIDOf.
-- * s - See, StateOf.
-- * i - See, SwitchInputOf.
-- * o - See, SwitchOutputOf.
-- * m - A variable that represents switch nesting.
data Switch k t s i o m

instance (RSAGL_FRP_FRPMODEL m, RSAGL_FRP_FRPMODEL_ENABLED_DISABLED k) => RSAGL_FRP_FRPMODEL (Switch k t s i o m) where

instance (RSAGL_FRP_FRPMODEL m, Eq t, RSAGL_FRP_FRPMODEL_ENABLED_DISABLED k) => FRPModel (Switch k t s i o m) where
    type ThreadingOf    (Switch k t s i o m) = k
    type ThreadIDOf     (Switch k t s i o m) = t
    type StateOf        (Switch k t s i o m) = s
    type SwitchInputOf  (Switch k t s i o m) = i
    type SwitchOutputOf (Switch k t s i o m) = o
    type Unwrap         (Switch k t s i o m) = m

type FRP1 s i o = Switch Disabled () s i o ()
type FRPX t s i o = FRPContext t i o (FRP1 s i [(t,o)])
type FRPContext t i o m = Switch Enabled t (SubState (StateOf m)) i o m
type FRP1Context i o m = Switch Disabled (ThreadIDOf m) (StateOf m) i o m

type SimpleSwitch k t s i o m = Switch k t s i o m

