{-# LANGUAGE Rank2Types #-}

module Roguestar.Lib.Behavior.Outcome
    (Effect(..),
     Outcome(..),
     FailureMode(..),
     OutcomeWithEffect(..),
     HasDuration(..))
    where

import Roguestar.Lib.DB
import qualified Data.Set as Set

-- | An effect or consequence in the game world.
class Effect e where
    applyEffect :: e -> DB ()

instance Effect (DB x) where
    applyEffect effect = effect >> return ()

-- | A failure mode: Abort means that the attempt was so obviously never possible that we should pretend the user (or AI, if an AI bug) never attempted.
-- An example of an Abort would be picking up an object that isn't nearby, or trying to go downstairs when there are no stairs.
data FailureMode = Success | Failure | Abort

-- | A primary outcome of a character's action.
class Outcome e where
    failureMode :: e -> FailureMode

instance (Effect e) => Effect [e] where
    applyEffect es = mapM_ applyEffect es

instance (Effect e) => Effect (Set.Set e) where
    applyEffect = applyEffect . Set.toList

instance (Effect a, Effect b) => Effect (a,b) where
    applyEffect (a,b) = applyEffect a >> applyEffect b

instance (Effect a, Effect b, Effect c) => Effect (a,b,c) where
    applyEffect (a,b,c) = applyEffect a >> applyEffect b >> applyEffect c

instance (Effect a) => Effect (Maybe a) where
    applyEffect = maybe (return ()) applyEffect

instance (Outcome a) => Outcome (Maybe a) where
    failureMode = maybe Abort failureMode

-- | An outcome (success or failure) with an effect (consequences of the outcome). If the outcome is also an effect (that is, it implements both classes),
-- then it must in both the outcome component and the effect component.
data OutcomeWithEffect o e = OutcomeWithEffect {
    _owe_outcome :: o,
    _owe_effect :: e,
    owe_duration :: (DBReadable db) => db Rational }

instance (Effect e) => Effect (OutcomeWithEffect o e) where
    applyEffect (OutcomeWithEffect _ e _) = applyEffect e

instance (Outcome o) => Outcome (OutcomeWithEffect o e) where
    failureMode (OutcomeWithEffect o _ _) = failureMode o

class HasDuration a where
    getDuration :: (DBReadable db) => a -> db Rational

instance HasDuration (OutcomeWithEffect o e) where
    getDuration = owe_duration

