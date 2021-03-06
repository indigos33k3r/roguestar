module Roguestar.Lib.Behavior.Activate
    (ActivationOutcome,
     resolveActivation,
     executeActivation)
    where

import Roguestar.Lib.Tool
import Roguestar.Lib.Data.ToolData
import Roguestar.Lib.Core.Monster
import Roguestar.Lib.DB
import Control.Monad.Error
import Control.Monad.Random
import Control.Monad.Reader
import Roguestar.Lib.Data.Substances

-- | Outcome of activating a tool.
data ActivationOutcome =
    Heal MonsterRef Integer
  | ExpendTool ToolRef ActivationOutcome
  | NoEffect

resolveActivation :: (MonadRandom db, DBReadable db) => MonsterRef -> db ActivationOutcome
resolveActivation creature_ref =
    do tool_ref <- maybe (throwError $ DBErrorFlag NoToolWielded) return =<< getWielded creature_ref
       tool <- asks $ getTool tool_ref
       case tool of
           DeviceTool {} -> throwError $ DBErrorFlag ToolIs_Innapropriate
           Sphere (ChromaliteSubstance c) ->
               do weightedPickM $ weightedSet [(1, ExpendTool tool_ref $ NoEffect),
                                               (chromaliteValue c, Heal creature_ref $ chromaliteValue c)]
           Sphere (MaterialSubstance m) ->
               do return $ ExpendTool tool_ref $ Heal creature_ref $ materialValue m
           Sphere (GasSubstance g) ->
               do weightedPickM $ weightedSet [(1, ExpendTool tool_ref $ NoEffect),
                                               (gasValue g, Heal creature_ref 1)]

executeActivation :: ActivationOutcome -> DB ()
executeActivation (NoEffect) = return ()
executeActivation (Heal creature_ref x) =
    do healMonster x creature_ref
       dbPushSnapshot $ HealEvent creature_ref
executeActivation (ExpendTool tool_ref activation_outcome) =
    do executeActivation activation_outcome
       dbPushSnapshot $ ExpendToolEvent tool_ref
       deleteTool tool_ref

