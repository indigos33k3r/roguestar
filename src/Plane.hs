{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings, PatternGuards, TypeFamilies #-}
module Plane
    (dbNewPlane,
     planetName,
     randomPlanetName,
     planeDepth,
     dbGetCurrentPlane,
     Plane.distanceBetweenSquared,
     pickRandomClearSite_withTimeout,
     pickRandomClearSite,
     getPlanarLocation,
     getBeneath,
     getSubsequent,
     terrainAt,
     setTerrainAt,
     whatIsOccupying,
     isTerrainPassable,
     getBiome)
    where

import Prelude hiding (getContents)
import Grids
import Reference
import DB
import TerrainData
import PlaneData
import PlanetData
import ToolData (Tool)
import BuildingData (Building)
import CreatureData (Creature)
import Control.Monad
import Data.Maybe
import Data.List
import Position
import PlayerState
import FactionData
import DetailedLocation
import qualified Data.ByteString.Char8 as B
import BuildingData
import Logging
import Control.Monad.Maybe
import Control.Monad.Trans

dbNewPlane :: (LocationConstructor l, ReferenceTypeOf l ~ Plane) => B.ByteString -> TerrainGenerationData -> l -> DB PlaneRef
dbNewPlane name tg_data l =
    do rns <- getRandoms
       random_id <- getRandomR (1,1000000)
       dbAddPlane (Plane { plane_biome = tg_biome tg_data,
                           plane_terrain = generateTerrain tg_data rns,
                           plane_random_id = random_id,
                           plane_planet_name = name}) l

planetName :: (DBReadable db) => PlaneRef -> db B.ByteString
planetName = liftM plane_planet_name . dbGetPlane

randomPlanetName :: (DBReadable db) => Faction -> db B.ByteString
randomPlanetName faction =
    do planet_number <- getRandomR (1000 :: Integer,9999)
       return $ factionPrefix faction `B.append` "-" `B.append` B.pack (show planet_number)

planeDepth :: (DBReadable db) => PlaneRef -> db Integer
planeDepth this_plane =
    do l <- whereIs this_plane
       case () of
           () | Just (Beneath above) <- fromLocation l -> liftM succ $ planeDepth above
           () | otherwise -> return 0

class AlwaysHasIndirectPlanarLocation a
instance AlwaysHasIndirectPlanarLocation Tool
instance AlwaysHasIndirectPlanarLocation Creature
instance AlwaysHasIndirectPlanarLocation Building

-- |
-- If this object is anywhere on a plane (such as carried by a creature who is on the plane),
-- returns the position of this object on that plane.
--
getPlanarLocation :: (DBReadable db,AlwaysHasIndirectPlanarLocation a) => Reference a -> db PlanarLocation
getPlanarLocation ref =
    liftM (fromMaybe (error "getPlanarLocation: Implements AlwaysHasIndirectPlanarLocation, but doesn't.") . listToMaybe . mapLocations) $ dbGetAncestors ref

-- |
-- Get the plane beneath this one, if it exists.
--
getBeneath :: (DBReadable db) => PlaneRef -> db (Maybe PlaneRef)
getBeneath item =
    do (plane_locs :: [DetailedLocation Beneath]) <- liftM mapLocations $ getContents item
       return $
           do Child plane_ref <- liftM detail $ listToMaybe plane_locs
              return plane_ref

-- |
-- Get the plane subsequent to this one, if it exists.
--
getSubsequent :: (DBReadable db) => PlanetRegion -> PlaneRef -> db (Maybe PlaneRef)
getSubsequent planet_region item =
    do plane_locs <- liftM (filterLocations $ \subsequent -> subsequent_via subsequent == planet_region) $ getContents item
       return $
           do Child plane_ref <- liftM detail $ listToMaybe plane_locs
              return plane_ref

-- |
-- Distance between two entities.  If the entities are not on the same plane, or for some
-- other reason it doesn't make sense to ask their distance, then Nothing.
--
distanceBetweenSquared :: (DBReadable db,
                            ReferenceType a,ReferenceType b,
                            AlwaysHasIndirectPlanarLocation a,
                            AlwaysHasIndirectPlanarLocation b) =>
    Reference a -> Reference b -> db (Maybe Integer)
distanceBetweenSquared a_ref b_ref =
    do a <- getPlanarLocation a_ref
       b <- getPlanarLocation b_ref
       (Parent a_parent :: Parent Plane, a_multiposition :: MultiPosition) <- liftM detail $ getPlanarLocation a_ref
       (Parent b_parent :: Parent Plane, b_multiposition :: MultiPosition) <- liftM detail $ getPlanarLocation b_ref
       return $
           do guard $ a_parent == b_parent
              return $ Position.distanceBetweenSquared a_multiposition b_multiposition

-- |
-- Gets the current plane of interest based on whose turn it is.
--
dbGetCurrentPlane :: (DBReadable db) => db (Maybe PlaneRef)
dbGetCurrentPlane = runMaybeT $
    do creature_with_current_turn <- MaybeT $ liftM creatureOf playerState
       (Parent plane_ref) <- liftM detail $ lift $ getPlanarLocation creature_with_current_turn
       return plane_ref

-- |
-- Selects sites at random until one seems reasonably clear.  It begins at
-- the specified Position on the map, and then picks more sites further and further away from the center
-- until it one seems clear -- this tends to concentrate sites near the center.
--
-- A site is considered clear if there are no objects at all within object_clear squares, and
-- only appropriate terrain (as defined by a predicate) within terrain_clear squares.
-- Distance is chessboard distance.
--
-- This function will gradually expand the search radius if encounters the slightest
-- difficulty finding a qualifying position.  The search radius parameter is strictly advisory.
--
-- This function can take an optional timeout parameter (pickRandomClearSite_withTimeout).  When used
-- without a timeout parameter, it may not terminate.  The only possible cause of non-termination is that no
-- site satisfies the terrain predicate.  However, if satisfactory sites are sufficiently rare,
-- extreme slowness is likely.
--
-- The timeout value should be a small integer greater or equal to one, since this function becomes slow with large timeout values.
--
pickRandomClearSite :: (DBReadable db) =>
    Integer -> Integer -> Integer ->
    Position -> (TerrainPatch -> Bool) -> PlaneRef ->
    db Position
pickRandomClearSite search_radius
                    object_clear
                    terrain_clear
                    p
                    terrainPredicate
                    plane_ref =
    liftM (fromMaybe $ error "pickRandomClearSite: impossible") $
        pickRandomClearSite_withTimeout Nothing
                                        search_radius
                                        object_clear
                                        terrain_clear
                                        p
                                        terrainPredicate
                                        plane_ref

pickRandomClearSite_withTimeout :: (DBReadable db) =>
    Maybe Integer -> Integer -> Integer -> Integer ->
    Position -> (TerrainPatch -> Bool) -> PlaneRef ->
    db (Maybe Position)
pickRandomClearSite_withTimeout (Just x) _ _ _ _ _ _ | x <= 0 = return Nothing
pickRandomClearSite_withTimeout timeout search_radius object_clear terrain_clear (Position (start_x,start_y)) terrainPredicate plane_ref =
    do logDB log_plane DEBUG $ "Searching for clear site . . ."
       xys <- liftM2 (\a b -> map Position $ zip a b)
           (mapM (\x -> liftM (+start_x) $ getRandomR (-x,x)) [1..search_radius])
           (mapM (\x -> liftM (+start_y) $ getRandomR (-x,x)) [1..search_radius])
       terrain <- liftM plane_terrain $ dbGetPlane plane_ref
       clutter_locations <- liftM (map identityDetail . filterLocations (\(_ :: MultiPosition) -> True)) $ getContents plane_ref
       let terrainIsClear (Position (x,y)) =
               all terrainPredicate $
                   concat [[gridAt terrain (x',y') |
                            x' <- [x-terrain_clear..x+terrain_clear]] |
                            y' <- [y-terrain_clear..y+terrain_clear]]
       let clutterIsClear here = not $ any (\p -> distanceBetweenChessboard here p <= object_clear) clutter_locations
       let m_result = find (\p -> terrainIsClear p && clutterIsClear p) xys
       case m_result of
           Just result ->
               do logDB log_plane DEBUG "Found clear site."
                  return $ Just result
           Nothing -> pickRandomClearSite_withTimeout
                          (fmap (subtract 1) timeout)
                          (search_radius + 1)
                          object_clear
                          (max 0 $ terrain_clear - 1)
                          (Position (start_x,start_y))
                          terrainPredicate
                          plane_ref

terrainAt :: (DBReadable db) => PlaneRef -> Position -> db TerrainPatch
terrainAt plane_ref (Position (x,y)) =
    do terrain <- liftM plane_terrain $ dbGetPlane plane_ref
       return $ gridAt terrain (x,y)

setTerrainAt :: PlaneRef -> Position -> TerrainPatch -> DB ()
setTerrainAt plane_ref (Position pos) patch = dbModPlane (\p -> p { plane_terrain = specificReplaceGrid pos patch $ plane_terrain p }) plane_ref

-- | Lists all of the entities that are on a specific spot, not including nested entities.
-- Typically this is zero or one creatures, and zero or more tools.  Might be a building.
whatIsOccupying :: (DBReadable db) => PlaneRef -> Position -> db [PlanarLocation]
whatIsOccupying plane_ref position =
    liftM (mapLocations . filterLocations (\(x :: MultiPosition) -> distanceBetweenChessboard position x == 0)) $ getContents plane_ref

-- | Answers True iff a creature may walk or swim or drop objects at the position.
-- Lava is considered passable, but trees are not.
isTerrainPassable :: (DBReadable db) => PlaneRef -> CreatureRef -> Position -> db Bool
isTerrainPassable plane_ref creature_ref position =
    do let f :: Maybe (Either (Child Building) (Child Creature)) -> Bool
           f = maybe False $ either (const True) (\(Child c) -> c /= creature_ref)
       (critters :: [PlanarLocation]) <- liftM (filter $ f . fromLocation . toLocation) $ whatIsOccupying plane_ref position
       terrain <- terrainAt plane_ref position
       return $ not (terrain `elem` [RockFace,Forest,DeepForest]) && null critters

getBiome :: (DBReadable db) => PlaneRef -> db Biome
getBiome = liftM plane_biome . dbGetPlane
