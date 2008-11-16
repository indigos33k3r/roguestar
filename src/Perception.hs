{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

-- |
-- Perception is essentially a catalogue of information that can be
-- observed from a creatures-eye-view, i.e. information that
-- is legal for a human agent or ai agent to have while choosing
-- it's next move.
--
module Perception
    (DBPerception,
     whoAmI,
     runPerception,
     visibleObjects,
     myFaction,
     Perception.getCreatureFaction,
     whereAmI,
     localBiome)
    where

import Control.Monad.Reader
import Control.Monad
import DB
import FactionData
import Creature
import PlaneVisibility
import PlaneData
import Data.Maybe
import Facing
import TerrainData

newtype (DBReadable db) => DBPerception db a = DBPerception { fromPerception :: (ReaderT CreatureRef db a) }

instance (DBReadable db) => Monad (DBPerception db) where
    (DBPerception a) >>= m = DBPerception $ a >>= (\x -> case m x of {(DBPerception b) -> b})
    return = DBPerception . return

instance (DBReadable db,MonadRandom db) => MonadRandom (DBPerception db) where
    getRandom = liftDB getRandom
    getRandoms = liftDB getRandoms
    getRandomR min_max = liftDB $ getRandomR min_max
    getRandomRs min_max = liftDB $ getRandomRs min_max

-- |
-- 'liftDB' takes an action in DBReadable and lifts it to DBPerception.  Obviously not exported,
-- or DBPerception wouldn't be limited.
--
liftDB :: (DBReadable db) => (forall m. DBReadable m => m a) -> DBPerception db a
liftDB actionM = DBPerception $ lift actionM

-- |
-- A run of DBPerception is tied to the creature doing the percieving.  'whoAmI' answers that creature.
-- We will call this creature "me" or "I".
--
whoAmI :: (DBReadable db) => DBPerception db CreatureRef
whoAmI = DBPerception $ ask

-- |
-- Run a DBPerception from the point-of-view of the given creature.
-- Note that if you pass any 'Reference' or 'Location' into the perception monad,
-- it will be able to cheat.  Therefore, don't.
--
runPerception :: (DBReadable db) => CreatureRef -> (forall m. DBReadable m => DBPerception m a) -> db a
runPerception creature_ref perception = dbSimulate $ runReaderT (fromPerception perception) creature_ref

visibleObjects :: (DBReadable db,LocationType a,LocationType b) => DBPerception db [Location S a b]
visibleObjects =
    do me <- whoAmI
       faction <- myFaction
       liftDB $ maybe (return []) (dbGetVisibleObjectsForFaction faction) =<< liftM extractLocation (dbWhere me)

myFaction :: (DBReadable db) => DBPerception db Faction
myFaction = Perception.getCreatureFaction =<< whoAmI

getCreatureFaction :: (DBReadable db) => CreatureRef -> DBPerception db Faction
getCreatureFaction creature_ref = liftDB $ Creature.getCreatureFaction creature_ref

whereAmI :: (DBReadable db) => DBPerception db (Facing,Position)
whereAmI = liftM (fromMaybe (error "whereAmI: I'm not on a plane") . extractLocation) $ whereIs =<< whoAmI

whatPlaneAmIOn :: (DBReadable db) => DBPerception db PlaneRef
whatPlaneAmIOn = liftM (fromMaybe (error "whatPlaneAmIOn: I'm not on a plane") . extractLocation) $ whereIs =<< whoAmI

whereIs :: (DBReadable db) => Reference a -> DBPerception db (Location S (Reference a) ())
whereIs ref = liftDB $ dbWhere ref

localBiome :: (DBReadable db) => DBPerception db Biome
localBiome = 
    do plane_ref <- whatPlaneAmIOn
       liftDB $ liftM plane_biome $ dbGetPlane plane_ref
