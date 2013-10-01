--Data
module Roguestar.Lib.Data.VisibilityData
    (distanceCostForSight,
     terrainHideMultiplier,
     terrainOpacity,
     maximumRangeForSpotCheck)
    where

import Roguestar.Lib.Data.TerrainData
import Data.List
import Roguestar.Lib.Data.FacingData

-- |
-- We multiply a creature's hide check by this number if it is standing on this terrain.
--
terrainHideMultiplier :: Terrain -> Integer
terrainHideMultiplier RockFace = 3
terrainHideMultiplier RockyGround = 0
terrainHideMultiplier Dirt = 0
terrainHideMultiplier Grass = 1
terrainHideMultiplier Sand = 1
terrainHideMultiplier Forest = 2
terrainHideMultiplier Water = 2
terrainHideMultiplier Ice = 0
terrainHideMultiplier Lava = 0  -- you definitely can't hide on lava
terrainHideMultiplier Glass = 0
terrainHideMultiplier RecreantFactory = 0
terrainHideMultiplier Downstairs = 2
terrainHideMultiplier Upstairs = 0
terrainHideMultiplier ForceField = 0  -- probably painful to be inside a force field, not good for stealth

-- |
-- We cast a ray between the spotter and the hider.  This indicates to what extent each terrain type
-- interferes with vision.
--
terrainOpacity :: Terrain -> Integer
terrainOpacity RockFace = 90
terrainOpacity RockyGround = 0
terrainOpacity Dirt = 0
terrainOpacity Grass = 5 -- sometimes there is tall grass
terrainOpacity Sand = 0
terrainOpacity Forest = 50
terrainOpacity Water = 0
terrainOpacity Ice = 0
terrainOpacity Lava = 10 -- lava makes smoke/refracts light due to intense heat?  Makes sense.
terrainOpacity Glass = 0
terrainOpacity RecreantFactory = 0
terrainOpacity Downstairs = 0
terrainOpacity Upstairs = 0
terrainOpacity ForceField = 10

-- |
-- The difficulty to spot an object at the given relative coordinates, taking facing into account.
--
distanceCostForSight :: Facing -> (Integer,Integer) -> Integer
distanceCostForSight facing (x,y) =
    let (xface,yface) = facingToRelative facing
        (x',y') = (x-xface,y-yface)
        in (x*x' + y*y')

-- |
-- The maximum distance from any point that a creature with that spot check could see anything,
-- no matter how well lit.
--
maximumRangeForSpotCheck :: Integer -> Integer
maximumRangeForSpotCheck spot_check = genericLength $ takeWhile (< spot_check) [((x+1)*(x+1)) | x <- [1..]]

