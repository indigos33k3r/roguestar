--Data
module Roguestar.Lib.Data.PlaneData
    (Plane(..))
    where

import Roguestar.Lib.Data.TerrainData
import qualified Data.ByteString.Char8 as B
import Roguestar.Lib.Random as Random

data Plane = Plane
    { plane_biome :: WeightedSet Biome,
      plane_terrain :: TerrainGrid,
      plane_random_id :: Integer,
      plane_planet_name :: B.ByteString }
    deriving (Read,Show)
