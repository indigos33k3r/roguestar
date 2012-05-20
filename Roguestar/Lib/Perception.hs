{-# LANGUAGE ExistentialQuantification, Rank2Types, FlexibleContexts, ScopedTypeVariables #-}

-- | The Perception monad is a wrapper for roguestar's core
-- monad that reveals only as much information as a character
-- legitimately has.  Thus, it is suitable for writing AI
-- routines as well as an API for the player's client.
module Roguestar.Lib.Perception
    (DBPerception,
     whoAmI,
     runPerception,
     visibleObjects,
     visibleTerrain,
     myFaction,
     Roguestar.Lib.Perception.getCreatureFaction,
     whereAmI,
     Roguestar.Lib.Perception.whereIs,
     localBiome,
     compass,
     depth)
    where

import Control.Monad.Reader
import Control.Monad.Random
import Data.Ord
import Roguestar.Lib.DB as DB
import Roguestar.Lib.Reference
import Roguestar.Lib.FactionData
import Roguestar.Lib.Creature as Creature
import Roguestar.Lib.PlaneVisibility
import Roguestar.Lib.PlaneData
import Data.Maybe
import Data.List
import Roguestar.Lib.Facing
import Roguestar.Lib.Position as Position
import Roguestar.Lib.TerrainData
import Roguestar.Lib.BuildingData
import Roguestar.Lib.Building
import Roguestar.Lib.Plane
import Roguestar.Lib.DetailedLocation
import Roguestar.Lib.Building

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
--
runPerception :: (DBReadable db) => CreatureRef -> (forall m. DBReadable m => DBPerception m a) -> db a
runPerception creature_ref perception = dbSimulate $ runReaderT (fromPerception perception) creature_ref

visibleTerrain :: (DBReadable db) => DBPerception db [(TerrainPatch,Position)]
visibleTerrain =
    do plane_ref <- whatPlaneAmIOn
       faction <- myFaction
       liftDB $ dbGetVisibleTerrainForFaction faction plane_ref

visibleObjects :: (DBReadable db) => (forall m. DBReadable m => Reference () -> DBPerception m Bool) -> DBPerception db [Location]
visibleObjects filterF =
    do me <- whoAmI
       faction <- myFaction
       m_parent_plane <- liftDB $ liftM fromLocation (DB.whereIs me)
       visible_objects <- case m_parent_plane of
           (Just (Parent plane_ref)) -> liftDB $ dbGetVisibleObjectsForFaction
                                            (\a -> runPerception me $ filterF a)
                                            faction
                                            plane_ref
           Nothing -> return []
       liftDB $ mapRO DB.whereIs visible_objects

myFaction :: (DBReadable db) => DBPerception db Faction
myFaction = Roguestar.Lib.Perception.getCreatureFaction =<< whoAmI

getCreatureFaction :: (DBReadable db) => CreatureRef -> DBPerception db Faction
getCreatureFaction creature_ref = liftDB $ Creature.getCreatureFaction creature_ref

whereAmI :: (DBReadable db) => DBPerception db (Facing,Position)
whereAmI = liftM detail $ Roguestar.Lib.Perception.whereIs =<< whoAmI

whatPlaneAmIOn :: (DBReadable db) => DBPerception db PlaneRef
whatPlaneAmIOn = liftM (planar_parent . identityDetail) $ (\x -> liftDB $ getPlanarLocation x) =<< whoAmI

whereIs :: (DBReadable db, ReferenceType a) =>
           Reference a -> DBPerception db (DetailedLocation (Child a))
whereIs ref = liftM (fromMaybe (error "Perception.whereIs: not a child of its own location record") . fromLocation) $ liftDB $ DB.whereIs ref

localBiome :: (DBReadable db) => DBPerception db Biome
localBiome =
    do plane_ref <- whatPlaneAmIOn
       liftDB $ liftM plane_biome $ dbGetPlane plane_ref

-- Let's look into re-writing this with A*:
-- http://hackage.haskell.org/packages/archive/astar/0.2.1/doc/html/Data-Graph-AStar.html
compass :: (DBReadable db) => DBPerception db Facing
compass =
    do (_,pos) <- whereAmI
       plane <- whatPlaneAmIOn
       liftDB $
           do (all_buildings :: [DetailedLocation (Child Building)]) <- liftM mapLocations $ DB.getContents plane
              all_signallers <- filterRO (liftM (== Just Magnetic) . buildingSignal . asChild . detail) all_buildings
              let multipositionOf :: DetailedLocation (Child Building) -> MultiPosition
                  multipositionOf = detail
                  sorted_signallers = sortBy (comparing $ Position.distanceBetweenSquared pos . multipositionOf) all_signallers
              return $ maybe Here (faceAt pos . detail) $ listToMaybe sorted_signallers

depth :: (DBReadable db) => DBPerception db Integer
depth =
    do plane <- whatPlaneAmIOn
       liftDB $ planeDepth plane

