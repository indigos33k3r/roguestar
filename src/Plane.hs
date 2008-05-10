
module Plane
    (dbNewPlane,
     dbGetCurrentPlane,
     dbDistanceBetweenSquared,
     pickRandomClearSite,
     getPlanarLocation)
    where

import Grids
import Dice
import DB
import DBData
import TerrainData
import PlaneData
import Control.Monad
import Data.Maybe
import Data.List
import Position

dbNewPlane :: TerrainGenerationData -> DB PlaneRef
dbNewPlane tg_data = 
    do rns <- dbNextRandomIntegerStream
       dbAddPlane (Plane { plane_terrain = generateTerrain tg_data rns }) ()

-- |
-- If this object is anywhere on a plane (such as carried by a creature who is on the plane),
-- returns the position of this object on that plane.
--
getPlanarLocation :: (DBReadable db,ReferenceType a) => Reference a -> db (Maybe (Location S (Reference ()) (PlaneRef,Position)))
getPlanarLocation ref =
    liftM (listToMaybe . mapMaybe coerceLocationRecord) $ dbGetAncestors ref

-- |
-- Distance between two entities.
--
dbDistanceBetweenSquared :: (DBReadable db,ReferenceType a,ReferenceType b) => Reference a -> Reference b -> db (Maybe Integer)
dbDistanceBetweenSquared a_ref b_ref =
    do m_a <- liftM (fmap location) $ getPlanarLocation a_ref
       m_b <- liftM (fmap location) $ getPlanarLocation b_ref
       return $
           do (p_a,a) <- m_a
	      (p_b,b) <- m_b
	      guard $ p_a == p_b
	      return $ distanceBetweenSquared a b

-- |
-- Gets the current plane of interest based on whose turn it is.
--
dbGetCurrentPlane :: (DBReadable db) => db (Maybe PlaneRef)
dbGetCurrentPlane = 
    do state <- playerState
       case state of
		  PlayerCreatureTurn creature_ref _ -> 
                      liftM (fmap $ fst . location) $ getPlanarLocation creature_ref
		  SnapshotEvent (AttackEvent { attack_event_source_creature = attacker_ref }) ->
		      liftM (fmap $ fst . location) $ getPlanarLocation attacker_ref
		  SnapshotEvent (MissEvent { miss_event_creature = attacker_ref }) ->
		      liftM (fmap $ fst . location) $ getPlanarLocation attacker_ref
		  SnapshotEvent (KilledEvent killed_ref) ->
		      liftM (fmap $ fst . location) $ getPlanarLocation killed_ref
		  _ -> return Nothing

-- |
-- Selects sites at random until one seems reasonably clear.  It begins at
-- the specified Position on the map, and then picks more sites further and further away from the center
-- until it one seems clear -- this tends to concentrate sites near the center.
--
-- A site is considered clear if there are no objects at all within object_clear squares, and
-- only appropriate terrain (as defined by a predicate) within terrain_clear squares.
--
-- This function will return an unsuitable site if it can't find a suitable one.
-- Such a site may have unsuitable terrain around it or it may be outside of
-- the search_radius (it is never impossible to find an area free of objects, since
-- terrain is infinite and objects are not).
--
pickRandomClearSite :: Integer -> Integer -> Integer -> Position -> (TerrainPatch -> Bool) -> PlaneRef -> DB Position
pickRandomClearSite search_radius object_clear terrain_clear (Position (start_x,start_y)) terrainPredicate plane_ref =
    do xys <- liftM2 (\a b -> map Position $ zip a b)
           (mapM (\x -> liftM (+start_x) $ roll [-x..x]) [1..search_radius])
           (mapM (\x -> liftM (+start_y) $ roll [-x..x]) [1..search_radius])
       terrain <- liftM plane_terrain $ dbGetPlane plane_ref
       clutter_locations <- locationsOf $ dbGetContents plane_ref
       let terrainIsClear (Position (x,y)) = 
               all terrainPredicate $
                   concat [[gridAt terrain (x',y') | 
                            x' <- [x-terrain_clear..x+terrain_clear]] |
			    y' <- [y-terrain_clear..y+terrain_clear]]
       let clutterIsClear (Position (x,y)) = not $ any (\(Position (x',y')) -> abs (x' - x) <= object_clear && y' - y <= object_clear) clutter_locations
       maybe (pickRandomClearSite (search_radius + 1) 
                                  object_clear 
                                  (max 0 $ terrain_clear - 1) 
                                  (Position (start_x,start_y))
				  terrainPredicate
				  plane_ref) 
             return $
             find (\p -> terrainIsClear p && clutterIsClear p) xys