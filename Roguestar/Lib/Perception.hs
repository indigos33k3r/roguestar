{-# LANGUAGE ExistentialQuantification, Rank2Types, FlexibleContexts, ScopedTypeVariables, PatternGuards #-}

--Utility
-- | The Perception monad is a wrapper for roguestar's core
-- monad that reveals only as much information as a character
-- legitimately has.  Thus, it is suitable for writingi "no-cheating"
-- AIs as well as an API for the human player's client.
--
module Roguestar.Lib.Perception
    (DBPerception,
     whoAmI,
     runPerception,
     VisibleObject(..),
     isVisibleTool,
     isVisibleMonster,
     isVisibleBuilding,
     stackVisibleObjects,
     visibleObjects,
     visibleTerrain,
     myFaction,
     myInventory,
     Roguestar.Lib.Perception.getMonsterFaction,
     whereAmI,
     Roguestar.Lib.Perception.whereIs,
     compass,
     depth,
     myHealth,
     Roguestar.Lib.Perception.isBehaviorAvailable)
    where

import Control.Monad.Reader
import Control.Monad.Random
import Data.Ord
import Roguestar.Lib.DB as DB
import Roguestar.Lib.Reference
import Roguestar.Lib.Data.FactionData
import Roguestar.Lib.Core.Monster as Monster
import Roguestar.Lib.PlaneVisibility
import Data.Maybe
import Data.List as List
import Data.Map as Map
import Control.Applicative
import Roguestar.Lib.Data.FacingData
import Roguestar.Lib.Position as Position
import Roguestar.Lib.TerrainData
import Roguestar.Lib.Data.BuildingData
import Roguestar.Lib.Core.Building
import Roguestar.Lib.Core.Plane
import Roguestar.Lib.Utility.DetailedLocation
import Roguestar.Lib.SpeciesData
import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.Tool
import Roguestar.Lib.ToolData
import Roguestar.Lib.Behavior as Behavior
import qualified Roguestar.Lib.Utility.DetailedTravel as DT

newtype (DBReadable db) => DBPerception db a = DBPerception { fromPerception :: (ReaderT MonsterRef db a) }

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
whoAmI :: (DBReadable db) => DBPerception db MonsterRef
whoAmI = DBPerception $ ask

-- |
-- Run a DBPerception from the point-of-view of the given creature.
--
runPerception :: (DBReadable db) => MonsterRef -> (forall m. DBReadable m => DBPerception m a) -> db a
runPerception creature_ref perception = dbSimulate $ runReaderT (fromPerception perception) creature_ref

visibleTerrain :: (DBReadable db) => DBPerception db [(Position,Terrain)]
visibleTerrain =
    do plane_ref <- whatPlaneAmIOn
       faction <- myFaction
       liftDB $ dbGetVisibleTerrainForFaction faction plane_ref

data VisibleObject =
    VisibleTool {
       visible_tool_ref :: ToolRef,
       visible_tool :: Tool,
       visible_object_position :: Position }
  | VisibleMonster {
       visible_creature_ref :: MonsterRef,
       visible_creature_species :: Species,
       visible_creature_traits :: Map.Map MonsterTrait Integer,
       visible_creature_wielding :: Maybe VisibleObject,
       visible_object_position :: Position,
       visible_creature_faction :: Faction }
  | VisibleBuilding {
       visible_building_ref :: BuildingRef,
       visible_building_shape :: BuildingShape,
       visible_building_occupies :: MultiPosition,
       visible_object_position :: Position }

isVisibleTool :: VisibleObject -> Bool
isVisibleTool (VisibleTool {}) = True
isVisibleTool _ = False

isVisibleMonster :: VisibleObject -> Bool
isVisibleMonster (VisibleMonster {}) = True
isVisibleMonster _ = False

isVisibleBuilding :: VisibleObject -> Bool
isVisibleBuilding (VisibleBuilding {}) = True
isVisibleBuilding _ = False

convertToVisibleObjectRecord :: (DBReadable db) => Reference a -> db VisibleObject
convertToVisibleObjectRecord ref | (Just creature_ref) <- coerceReference ref =
    do species <- liftM creature_species $ dbGetMonster creature_ref
       traits <- liftM creature_traits $ dbGetMonster creature_ref
       faction <- Monster.getMonsterFaction creature_ref
       m_tool_ref <- getWielded creature_ref
       position <- liftM detail $ DT.whereIs creature_ref
       m_wielded <- case m_tool_ref of
           Just tool_ref ->
               do tool <- dbGetTool tool_ref
                  return $ Just $ VisibleTool tool_ref tool position
           Nothing -> return Nothing
       return $ VisibleMonster creature_ref species traits m_wielded position faction
convertToVisibleObjectRecord ref | (Just tool_ref) <- coerceReference ref =
    do tool <- dbGetTool tool_ref
       position <- liftM detail $ getPlanarLocation tool_ref
       return $ VisibleTool tool_ref tool position
convertToVisibleObjectRecord ref | (Just building_ref :: Maybe BuildingRef) <- coerceReference ref =
    do location <- DT.whereIs building_ref
       return $ VisibleBuilding building_ref (detail location) (detail location) (detail location)
convertToVisibleObjectRecord _ | otherwise = error "convertToVisibleObjectRecord: Impossible case."

-- |
-- Takes a list of VisibleObjects and arranges them by their
-- position in sorted order.
--
-- The sort order should put the most "important" object
-- on top.  For example, if a creature and a tool
-- both occupy a square, it is more important to display
-- the creature than the tool.
--
stackVisibleObjects :: [VisibleObject] -> Map Position [VisibleObject]
stackVisibleObjects = List.foldr insertVob Map.empty
    where insertVob :: VisibleObject -> Map Position [VisibleObject] -> Map Position [VisibleObject]
          insertVob vob = List.foldr (\k f -> Map.alter (insertVob_ vob) k . f)
                                     id
                                     (fromMultiPosition $ visibleObjectPosition vob)
          insertVob_ :: VisibleObject -> Maybe [VisibleObject] -> Maybe [VisibleObject]
          insertVob_ vob m_vobs =
              (do vobs <- m_vobs
                  return $ sortBy (comparing $ negate . visibleObjectSize) $ vob:vobs)
            <|>
                  return [vob]

-- |
-- Get the position of a visible object.
--
visibleObjectPosition :: VisibleObject -> MultiPosition
visibleObjectPosition (VisibleBuilding { visible_building_occupies = multi_position }) = multi_position
visibleObjectPosition vob = toMultiPosition $ visible_object_position vob

visibleObjectSize :: VisibleObject -> Integer
visibleObjectSize (VisibleTool {} ) = 0
visibleObjectSize _ = 1000000

visibleObjects :: (DBReadable db) =>
                  (forall m. DBReadable m => Reference () -> DBPerception m Bool) ->
                  DBPerception db [VisibleObject]
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
       liftDB $ mapRO convertToVisibleObjectRecord visible_objects

myInventory :: (DBReadable db) => DBPerception db [VisibleObject]
myInventory =
    do me <- whoAmI
       (result :: [DetailedLocation Inventory]) <- liftDB $ liftM mapLocations $ DB.getContents me
       liftDB $ mapRO convertToVisibleObjectRecord $ sortBy (comparing toUID) $ (asChildren result :: [ToolRef])

myFaction :: (DBReadable db) => DBPerception db Faction
myFaction = Roguestar.Lib.Perception.getMonsterFaction =<< whoAmI

getMonsterFaction :: (DBReadable db) => MonsterRef -> DBPerception db Faction
getMonsterFaction creature_ref = liftDB $ Monster.getMonsterFaction creature_ref

whereAmI :: (DBReadable db) => DBPerception db (Facing,Position)
whereAmI = liftM detail $ Roguestar.Lib.Perception.whereIs =<< whoAmI

whatPlaneAmIOn :: (DBReadable db) => DBPerception db PlaneRef
whatPlaneAmIOn = liftM (planar_parent . identityDetail) $ (\x -> liftDB $ getPlanarLocation x) =<< whoAmI

whereIs :: (DBReadable db, ReferenceType a) =>
           Reference a -> DBPerception db (DetailedLocation (Child a))
whereIs ref = liftM (fromMaybe (error "Perception.whereIs: not a child of its own location record") . fromLocation) $ liftDB $ DB.whereIs ref

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

-- |
-- Depth of the current plane below the surface.
--
depth :: (DBReadable db) => DBPerception db Integer
depth =
    do plane <- whatPlaneAmIOn
       liftDB $ planeDepth plane

myHealth :: (DBReadable db) => DBPerception db MonsterHealth
myHealth =
    do creature_ref <- whoAmI
       liftDB $ getMonsterHealth creature_ref

isBehaviorAvailable :: (DBReadable db) => Behavior -> DBPerception db Bool
isBehaviorAvailable b =
    do creature_ref <- whoAmI
       liftDB $ Behavior.isBehaviorAvailable b creature_ref

