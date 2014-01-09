--Data
module Roguestar.Lib.Data.PlaneData
    (PlaneData(..))
    where

import Roguestar.Lib.Data.TerrainData
import qualified Data.ByteString.Char8 as B
import Roguestar.Lib.Random as Random

data PlaneData = PlaneData
    { plane_biome :: WeightedSet Biome,   -- TODO: Get rid of this.
      plane_terrain :: TerrainGrid,       -- TODO: Use a persistable domain-specific language to procedurally generate these grids
      plane_random_id :: Integer,         -- Just a random number
      plane_planet_name :: B.ByteString } -- Human-readable name of the planet. TODO: switch to Text instead of ByteString. TODO: this is stored redundantly on multiple planes belonging to the same planet?
    deriving (Read,Show)
