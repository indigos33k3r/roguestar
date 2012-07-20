{-# LANGUAGE ExistentialQuantification, Rank2Types, FlexibleContexts, ScopedTypeVariables, PatternGuards #-}

-- | The Perception monad is a wrapper for roguestar's core
-- monad that reveals only as much information as a character
-- legitimately has.  Thus, it is suitable for writing AI
-- routines as well as an API for the human player's client.
module Roguestar.Lib.Perception
    (DBPerception,
     whoAmI,
     runPerception,
     VisibleObject(..),
     isVisibleTool,
     isVisibleCreature,
     isVisibleBuilding,
     stackVisibleObjects,
     visibleObjects,
     visibleTerrain,
     myFaction,
     myInventory,
     Roguestar.Lib.Perception.getCreatureFaction,
     whereAmI,
     Roguestar.Lib.Perception.whereIs,
     localBiome,
     compass,
     depth,
     myHealth)
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
import Data.List as List
import Data.Map as Map
import Control.Applicative
import Roguestar.Lib.Facing
import Roguestar.Lib.Position as Position
import Roguestar.Lib.TerrainData
import Roguestar.Lib.BuildingData
import Roguestar.Lib.Building
import Roguestar.Lib.Plane
import Roguestar.Lib.DetailedLocation
import Roguestar.Lib.Building
import Roguestar.Lib.SpeciesData
import qualified Data.ByteString.Char8 as B
import Roguestar.Lib.CreatureData
import qualified Data.Set as Set
import qualified Data.Map as Map
import Roguestar.Lib.Tool
import Roguestar.Lib.ToolData
import Roguestar.Lib.PersistantData
import qualified Roguestar.Lib.DetailedTravel as DT

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

data VisibleObject =
    VisibleTool {
       visible_tool_ref :: ToolRef,
       visible_tool :: Tool,
       visible_object_position :: Position }
  | VisibleCreature {
       visible_creature_ref :: CreatureRef,
       visible_creature_species :: Species,
       visible_creature_character_classes :: [CharacterClass],
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

isVisibleCreature :: VisibleObject -> Bool
isVisibleCreature (VisibleCreature {}) = True
isVisibleCreature _ = False

isVisibleBuilding :: VisibleObject -> Bool
isVisibleBuilding (VisibleBuilding {}) = True
isVisibleBuilding _ = False

convertToVisibleObjectRecord :: (DBReadable db) => Reference a -> db VisibleObject
convertToVisibleObjectRecord ref | (Just creature_ref) <- coerceReference ref =
    do species <- liftM creature_species $ dbGetCreature creature_ref
       classes <- liftM (Map.keys . creature_levels) $ dbGetCreature creature_ref
       faction <- Creature.getCreatureFaction creature_ref
       m_tool_ref <- getWielded creature_ref
       position <- liftM detail $ DT.whereIs creature_ref
       m_wielded <- case m_tool_ref of
           Just tool_ref ->
               do tool <- dbGetTool tool_ref
                  return $ Just $ VisibleTool tool_ref tool position
           Nothing -> return Nothing
       return $ VisibleCreature creature_ref species classes m_wielded position faction
convertToVisibleObjectRecord ref | (Just tool_ref) <- coerceReference ref =
    do tool <- dbGetTool tool_ref
       position <- liftM detail $ getPlanarLocation tool_ref
       return $ VisibleTool tool_ref tool position
convertToVisibleObjectRecord ref | (Just building_ref :: Maybe BuildingRef) <- coerceReference ref =
    do location <- DT.whereIs building_ref
       return $ VisibleBuilding building_ref (detail location) (detail location) (detail location)

stackVisibleObjects :: [VisibleObject] -> Map Position [VisibleObject]
stackVisibleObjects = foldr insertVob Map.empty
    where insertVob :: VisibleObject -> Map Position [VisibleObject] -> Map Position [VisibleObject]
          insertVob vob = foldr (\k f -> Map.alter (insertVob_ vob) k . f)
                                id
                                (fromMultiPosition $ visibleObjectPosition vob)
          insertVob_ :: VisibleObject -> Maybe [VisibleObject] -> Maybe [VisibleObject]
          insertVob_ vob m_vobs =
              (do vobs <- m_vobs
                  return $ sortBy (comparing $ negate . visibleObjectSize) $ vob:vobs)
            <|>
                  return [vob]

visibleObjectPosition :: VisibleObject -> MultiPosition
visibleObjectPosition (VisibleBuilding { visible_building_occupies = multi_position }) = multi_position
visibleObjectPosition vob = toMultiPosition $ visible_object_position vob

visibleObjectSize :: VisibleObject -> Integer
visibleObjectSize (VisibleTool { visible_tool = t } ) = 0
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
       
myHealth :: (DBReadable db) => DBPerception db CreatureHealth
myHealth =
    do creature_ref <- whoAmI
       liftDB $ getCreatureHealth creature_ref

