{-# LANGUAGE ExistentialQuantification #-}
module Roguestar.Lib.Utility.SiteCriteria
    (SiteCriteria(..),
     SimpleSiteCriteria,
     areaClearForObjectPlacement,
     onTerrainType,
     closeTo,
     atDistanceFrom,
     pickRandomSite)
    where

import Data.Ord
import Data.List as List
import Roguestar.Lib.Core.Plane
import Roguestar.Lib.DB
import Roguestar.Lib.Data.TerrainData
import Control.Monad
import Control.Monad.Random

-- |
-- Criteria for randomly choosing sites to place things on a plane.
-- As a simple example, a building should randomly put on a site where there are not already any buildings.
class SiteCriteria a where
    testSiteCriteria :: (DBReadable db) => PlaneRef -> Position -> a -> db Double

data SimpleSiteCriteria =
    TerrainClear { _terrain_clear_radius :: Integer,
                   _terrain_clear_test :: Terrain -> Bool } |
    ObjectClear { _object_clear_radius :: Integer } |
    AtDistanceFrom { _at_distance_from_center :: Position,
                     _at_distance :: Integer } |
    forall a. SiteCriteria a => RequireAtLeast { require_at_least :: Double, require_at_least_criteria ::  a }

instance SiteCriteria SimpleSiteCriteria where
    testSiteCriteria plane_ref (Position (x,y)) (TerrainClear radius testF) =
        do let ps = [Position (x',y') | x' <- [x-radius..x+radius], y' <- [y-radius..y+radius]]
               p_count = realToFrac $ length ps
           liftM sum $ forM ps $ \p ->
               do t <- terrainAt plane_ref p
                  case testF t of
                      True -> return $ 1/p_count
                      False -> return $ -1/p_count
    testSiteCriteria plane_ref (Position (x,y)) (ObjectClear radius) =
        do let ps = [Position (x',y') | x' <- [x-radius..x+radius], y' <- [y-radius..y+radius]]
               p_count = realToFrac $ length ps
           liftM sum $ forM ps $ \p ->
               do o <- whatIsOccupying plane_ref p
                  case o of
                      [] -> return $ 1/p_count
                      _  -> return $ -1/p_count
    testSiteCriteria _ (Position (x,y)) (AtDistanceFrom (Position (x',y')) distance) = return $ 1.0 / (abs $ sqrt (fromInteger ((x-x')^2 + (y-y')^2)) - fromInteger distance)
    testSiteCriteria plane_ref p require@(RequireAtLeast { require_at_least_criteria = criteria }) =
        do result <- testSiteCriteria plane_ref p criteria
           case result > require_at_least require of
               False -> return $ result-1e6
               True  -> return result

-- SiteCriteria that requires a radius in which there should be no other buildings, objects, or impassable terrain.
areaClearForObjectPlacement :: Integer -> SimpleSiteCriteria
areaClearForObjectPlacement radius = RequireAtLeast 0.999 $ [TerrainClear radius (not . (`elem` difficult_terrains)), ObjectClear radius]

-- SiteCriteria that requires the found site to exactly match the specified type of terrain patch.
onTerrainType :: Terrain -> SimpleSiteCriteria
onTerrainType terrain = RequireAtLeast 0 $ TerrainClear 0 (== terrain)

-- SiteCriteria that tries to get as close to the specified position as possible.
closeTo :: Position -> SimpleSiteCriteria
closeTo p = AtDistanceFrom p 0

-- SiteCriteria that tries to get at a specific distance from the specified position.
atDistanceFrom :: Position -> Integer -> SimpleSiteCriteria
atDistanceFrom p d = AtDistanceFrom p d

instance SiteCriteria a => SiteCriteria [a] where
    testSiteCriteria plane_ref p xs = liftM sum $ mapM (testSiteCriteria plane_ref p) xs

pickRandomSite :: (DBReadable db, SiteCriteria a) => (Integer,Integer) -> (Integer,Integer) -> Integer -> a -> PlaneRef -> db Position
pickRandomSite east_west north_south tryhard site_criteria plane_ref =
        do liftM pickBest $ forM [1.. fromInteger tryhard] $ const generateOption
    where pickBest :: [(Double,Position)] -> Position
          pickBest = snd . maximumBy (comparing fst)
          generateOption :: (DBReadable db) => db (Double,Position)
          generateOption =
              do x <- getRandomR east_west
                 y <- getRandomR north_south
                 let p = Position (x,y)
                 fitness <- testSiteCriteria plane_ref p site_criteria
                 return (fitness,p)

