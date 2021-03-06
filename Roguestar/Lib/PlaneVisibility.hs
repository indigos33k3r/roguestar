{-# LANGUAGE PatternGuards, FlexibleContexts, ScopedTypeVariables, RankNTypes #-}

--Utility
module Roguestar.Lib.PlaneVisibility
    (dbGetVisibleTerrainForFaction,
     dbGetVisibleObjectsForFaction)
    where

import Prelude hiding (getContents)
import Roguestar.Lib.Data.FactionData
import Roguestar.Lib.DB
import Roguestar.Lib.Data.TerrainData
import Roguestar.Lib.Core.Plane
import Roguestar.Lib.Data.PlaneData
import Control.Monad
import Control.Monad.Random
import Control.Monad.Reader
import Roguestar.Lib.Data.MonsterData
import Data.List as List
import Roguestar.Lib.Utility.Grids
import Roguestar.Lib.Utility.RayCasting
import Roguestar.Lib.Data.VisibilityData
import Roguestar.Lib.Data.FacingData
import Data.Ratio
import Roguestar.Lib.Core.Building
import Roguestar.Lib.Position as Position
import Control.Applicative
import Roguestar.Lib.Utility.DetailedLocation

dbGetSeersForFaction :: (DBReadable db) => Faction -> PlaneRef -> db [MonsterRef]
dbGetSeersForFaction faction plane_ref =
    filterM (filterByFaction faction) =<< liftM asChildren (asks $ getContents plane_ref)

-- |
-- Returns a list of all terrain patches that are visible to any creature belonging
-- to the specified faction on the specified plane.
--
dbGetVisibleTerrainForFaction :: (DBReadable db) => Faction -> PlaneRef -> db [(Position,Terrain)]
dbGetVisibleTerrainForFaction faction plane_ref =
    do critters <- dbGetSeersForFaction faction plane_ref
       liftM (nub . concat) $ mapRO dbGetVisibleTerrainForMonster critters

-- |
-- Returns a list of all terrain patches that are visible to the specified creature.
--
dbGetVisibleTerrainForMonster :: (DBReadable db) => MonsterRef -> db [(Position,Terrain)]
dbGetVisibleTerrainForMonster creature_ref =
    do loc <- liftM identityDetail $ getPlanarLocation creature_ref
       spot_check <- dbGetSpotCheck creature_ref
       liftM (visibleTerrain (planar_position loc) spot_check . plane_terrain) $ asks $ getPlane (planar_parent loc)

-- |
-- Returns a list of all objects that are visible to any creature belonging
-- to the specified faction on the specified plane.  Accepts a filter to
-- determine what kinds of objects will be tested.
--
dbGetVisibleObjectsForFaction :: (MonadRandom db, DBReadable db) => (forall m. (MonadRandom m, DBReadable m) => Reference () -> m Bool) -> Faction -> PlaneRef -> db [Reference ()]
dbGetVisibleObjectsForFaction filterF faction plane_ref =
    do critters <- dbGetSeersForFaction faction plane_ref
       liftM (nubBy (=:=) . concat) $ mapM (dbGetVisibleObjectsForMonster (\x -> ro $ filterF x)) critters

-- |
-- Returns a list of all objects that are visible to the specified creature.
-- Accepts a filter to determine what kinds of objects will be tested.
--
dbGetVisibleObjectsForMonster :: (DBReadable db) => (forall m. DBReadable m => Reference () -> m Bool) -> MonsterRef -> db [Reference ()]
dbGetVisibleObjectsForMonster filterF creature_ref =
    do plane_ref <- liftM (planar_parent . identityDetail) $ getPlanarLocation creature_ref
       possibles <- liftM asChildren $ asks $ getContents plane_ref
       filterRO (\a -> (&&) <$> filterF a <*> dbIsPlanarVisible creature_ref a) possibles
-- |
-- dbIsPlanarVisible (a creature) (some object) is true if the creature can see the object.
--
dbIsPlanarVisible :: (DBReadable db,ReferenceType a) => MonsterRef -> Reference a -> db Bool
dbIsPlanarVisible creature_ref obj_ref | creature_ref =:= obj_ref = return True
dbIsPlanarVisible creature_ref obj_ref =
    do c <- liftM identityDetail $ getPlanarLocation creature_ref
       (m_o :: Maybe Planar) <- liftM fromLocation $ asks $ whereIs obj_ref
       spot_check <- dbGetOpposedSpotCheck creature_ref obj_ref
       case m_o of
            Nothing -> return False
            Just o | planar_parent c /= planar_parent o -> return False --never see objects on different planes
            Just o | distanceBetweenChessboard (planar_position c) (planar_multiposition o) <= 1 -> return True --automatically see 8-adjacent objects
            Just o | Position.distanceBetweenSquared (planar_position c) (planar_multiposition o) > (maximumRangeForSpotCheck spot_check)^2 -> return False --cull objects that are too far away to ever be seen
            Just o -> liftM or $ forM (positionPairs (planar_position c) (planar_multiposition o)) $
                \(Position (cx,cy),Position (ox,oy)) ->
                    do let delta_at = (ox-cx,oy-cy)
                       terrain <- liftM plane_terrain $ asks $ getPlane (planar_parent c) -- falling through all other tests, cast a ray for visibility
                       return $ castRay (cx,cy) (ox,oy) (spot_check - distanceCostForSight Here delta_at) (terrainOpacity . (\(Terrain t) -> t) . gridAt terrain)

dbGetOpposedSpotCheck :: (DBReadable db) => MonsterRef -> Reference a -> db Integer
dbGetOpposedSpotCheck creature_ref object_ref =
    do spot <- dbGetSpotCheck creature_ref
       hide <- dbGetHideCheck object_ref
       return $ round $ (spot%1) * opposedLinearPowerRatio spot hide

planarLightingBonus :: (DBReadable db) => PlaneRef -> db Integer
planarLightingBonus = liftM (\x -> max 0 $ 25 - x*5) . planeDepth

dbGetSpotCheck :: (DBReadable db) => MonsterRef -> db Integer
dbGetSpotCheck creature_ref =
    do plane_ref <- liftM (planar_parent . identityDetail) $ getPlanarLocation creature_ref
       bonus <- planarLightingBonus $ plane_ref
       ability_score <- liftM (creatureAbilityScore SpotSkill) $ asks $ getMonster creature_ref
       return $ ability_score + bonus

dbGetHideCheck :: (DBReadable db) => Reference a -> db Integer
dbGetHideCheck ref | Just (creature_ref :: MonsterRef) <- coerceReference ref = liftM (creatureAbilityScore HideSkill) $ asks $ getMonster creature_ref
dbGetHideCheck ref | Just (building_ref :: BuildingRef) <- coerceReference ref = liftM negate $ buildingSize building_ref
dbGetHideCheck _   | otherwise = return 1

-- |
-- visibleTerrain (creature's location) (spot check) (the terrain map) gives
-- a list of visible terrain patches from that location with that spot check.
--
visibleTerrain :: Position -> Integer -> TerrainGrid -> [(Position,Terrain)]
visibleTerrain (Position (creature_at@(creature_x,creature_y))) spot_check terrain =
    let max_range = maximumRangeForSpotCheck spot_check
        in List.map (\(x,y) -> (Position (x,y),(\(Terrain t) -> t) $ gridAt terrain (x,y))) $
           castRays creature_at
                        [terrainPatchBrightnessForm creature_at spot_check (creature_x+x,creature_y+y)
                         | x <- [-max_range..max_range],
                         y <- [-max_range..max_range],
                         x^2+y^2 <= max_range^2]
                        (terrainOpacity . (\(Terrain t) -> t) . gridAt terrain)

-- |
-- terrainPatchBrightnessForm (creature's location) (spot check) (terrain patch's location)
-- gives (the patch's location,the patch's effective brightness) for the purpose of applying castRays.
--
terrainPatchBrightnessForm :: (Integer,Integer) -> Integer -> (Integer,Integer) -> ((Integer,Integer),Integer)
terrainPatchBrightnessForm creature_at spot_check patch_at =
    let delta_at = (fst creature_at - fst patch_at,snd creature_at - snd patch_at)
        in (patch_at,spot_check - distanceCostForSight Here delta_at)

-- |
-- Returns true if the specified MonsterRef belongs to the specified Faction.
--
filterByFaction :: (DBReadable db) => Faction -> MonsterRef -> db Bool
filterByFaction faction = liftM ((== faction) . creature_faction) . asks . getMonster
