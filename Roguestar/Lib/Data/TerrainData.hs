--Data
module Roguestar.Lib.Data.TerrainData
    (Biome(..),
     Terrain(..),
     MetaTerrain(..),
     TerrainGrid,
     TerrainGenerationData(..),
     TerrainPlacement,
     recreantFactories,
     stairsUp,
     stairsDown,
     generateTerrain,
     difficult_terrains,
     impassable_terrains)
    where

import Roguestar.Lib.Utility.Grids
import Data.List as List
import Data.Map as Map
--import Substances hiding (Water)
import Roguestar.Lib.Random as Random
import Control.Arrow (first,second)

-- |
-- Biomes represent terrain phenomenon.  A terrain map will interpolate between biomes.
--
data Biome = TemperateForest
           | TemperateClearing
           | TropicalForest
           | TropicalClearing
           | RelaxingPond
           | BareMountain
           | CraterInterior
           | LavaPool
             deriving (Read,Show,Eq,Ord,Enum,Bounded)

-- |
-- Specific terrain 'squares' out of which the terrain map is constructed.
--
data Terrain = RockFace
             | RockyGround
             | Dirt
             | Grass
             | Sand
             | Forest
             | Water
             | Ice
             | Lava
             | Glass -- solidified lava
             | RecreantFactory
             | ForceField
             | Upstairs
             | Downstairs
                deriving (Read,Show,Eq,Ord,Enum,Bounded)

data MetaTerrain = Biome Biome | Terrain Terrain
                deriving (Read,Show,Eq,Ord)

data TerrainGenerationData = TerrainGenerationData
                           { tg_smootheness :: Integer,
                             tg_biome :: WeightedSet Biome,
                             tg_placements :: [TerrainPlacement] }
                           deriving (Read,Show)

data TerrainPlacement = TerrainPlacement {
    placement_sources :: [(Double,Terrain)],
    placement_replacements :: WeightedSet Terrain,
    placement_seed :: Integer,
    placement_blob :: Blob }
        deriving (Read,Show)

placeTerrain :: TerrainPlacement -> TerrainGrid -> TerrainGrid
placeTerrain terrain_placement =
    arbitraryReplaceGrid (List.map (second Terrain) $ placement_sources terrain_placement)
                         (fmap Terrain $ placement_replacements terrain_placement)
                         (placement_seed terrain_placement)
                         (placement_blob terrain_placement)

recreantFactories :: Integer -> TerrainPlacement
recreantFactories seed = TerrainPlacement {
    placement_sources =
        [(1/2,Ice),
         (1/10,Sand),
         (1/5,Dirt),
         (1/1,Glass),
         (1/10,Grass),
         (1/100,Forest),
         (1/2,RockyGround)],
    placement_replacements =
        unweightedSet [RecreantFactory],
    placement_seed = seed,
    placement_blob = ConeBlob (0,0) 100 }

stairsUp :: Integer -> Integer -> TerrainPlacement
stairsUp seed depth = TerrainPlacement {
    placement_sources =
        [(1/(15+3*realToFrac depth),RockyGround),
         (1/(25+5*realToFrac depth),Ice),
         (1/(50+10*realToFrac depth),Water),
         (1/(75+15*realToFrac depth),RockFace)],
    placement_replacements =
        unweightedSet [Upstairs],
    placement_seed = seed,
    placement_blob = UnitBlob }

stairsDown :: Integer -> Integer -> TerrainPlacement
stairsDown seed depth = TerrainPlacement {
    placement_sources =
        [(1/(15+4*realToFrac depth),RockyGround),
         (1/(25+5*realToFrac depth),Ice),
         (1/(75+3*realToFrac depth),RockFace),
         (1/(40+10*realToFrac depth),Dirt),
         (1/60,Grass)],
    placement_replacements =
        unweightedSet[Downstairs],
    placement_seed = seed,
    placement_blob = UnitBlob }

-- |
-- A list of TerrainPatches that are considered "difficult", either for traveling
-- or for constructing buildings.
--
difficult_terrains :: [Terrain]
difficult_terrains = impassable_terrains ++ [Water,Lava]

-- |
-- A list of TerrainPatches that are considered "impassable" for traveling.
--
impassable_terrains :: [Terrain]
impassable_terrains = [RockFace,Forest,ForceField]

terrainInterpFn :: (Biome,Biome) -> WeightedSet Terrain
terrainInterpFn biomes = case biomes of
    (TemperateForest,_)             -> weightedSet [(2,Forest),(1,Grass),(1,Dirt)]
    (TemperateClearing,_)           -> weightedSet [(5,Grass),(3,Dirt)]
    (TropicalForest,_)              -> weightedSet [(3,Forest),(1,Sand),(1,Water)]
    (TropicalClearing,_)            -> weightedSet [(3,Grass),(2,Sand),(1,Water)]
    (BareMountain,BareMountain)     -> weightedSet [(1,RockFace),(1,RockyGround)]
    (BareMountain,_)                -> weightedSet [(1,RockyGround)]
    (RelaxingPond,RelaxingPond)     -> weightedSet [(10,Water),(1,RockyGround)]
    (RelaxingPond,_)                -> weightedSet [(2,Water),(1,Sand)]
    (CraterInterior,CraterInterior) -> weightedSet [(1,RockyGround)]
    (CraterInterior,_)              -> weightedSet [(1,RockFace)]
    (LavaPool,LavaPool)             -> weightedSet [(2,Lava),(1,Glass)]
    (LavaPool,_)                    -> weightedSet [(1,RockFace),(1,Glass)]

terrainInterpMap :: Map (MetaTerrain,MetaTerrain) (WeightedSet MetaTerrain)
terrainInterpMap =
    let biome_patch_pairs :: [(Biome,Biome)]
        biome_patch_pairs = [(a,b) | a <- [minBound..maxBound], b <- [minBound..maxBound]]
        interps = List.map (fmap Terrain . terrainInterpFn) biome_patch_pairs
            in fromList (zip (List.map (first Biome . second Biome) biome_patch_pairs) interps)

type TerrainGrid = Grid MetaTerrain

-- |
-- Generates a random terrain map.  The Biome indicates determines what TerrainPatches
-- are generated.  The second parameter is an Integer that indicates the smootheness of the
-- generated terrain.  Finally, a random Integer stream is needed to provide the random data
-- to generate the terrain.
--
generateTerrain :: TerrainGenerationData -> [Integer] -> TerrainGrid
generateTerrain tg rands = flip (List.foldr placeTerrain) (tg_placements tg) $
    interpolateGrid Nothing (head rands) $
    interpolateGrid (Just terrainInterpMap) (head $ drop 1 rands) $
    generateGrid (fmap Biome $ tg_biome tg) Nothing (tg_smootheness tg) (drop 2 rands)

