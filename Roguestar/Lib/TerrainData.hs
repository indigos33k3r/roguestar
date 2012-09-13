
module Roguestar.Lib.TerrainData
    (Biome(..),
     TerrainPatch(..),
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

import Roguestar.Lib.Grids
import Data.List as List
import Data.Map as Map
--import Substances hiding (Water)
import Roguestar.Lib.RNG


-- |
-- Most automatically generated surface maps belong to a Biome, representing the kind of terrain
-- and plant life that dwells in terrain generated for the map.
--
data Biome = ShallowDungeon
           | DeepDungeon
           | FrozenDungeon
           | AbyssalDungeon
           | InfernalDungeon
           | RockBiome
           | IcyRockBiome
           | GrasslandBiome
           | ForestBiome
           | TundraBiome
           | DesertBiome
           | OceanBiome
           | MountainBiome
           | SwampBiome
           | PolarBiome
             deriving (Read,Show,Eq,Ord,Enum,Bounded)

-- |
-- All static terrain elements are members of TerrainGrid
--
-- The only difference between "Deasert" and "Sand" is that where
-- "Deasert" and "Water" touch, the map generator will produce
-- patches of plantlife (for oasis and shoreline effect).
--
data TerrainPatch = RockFace
                  | Rubble
                  | Ore
                  | RockyGround
                  | Dirt
                  | Grass
                  | Sand
                  | Desert -- exactly like sand, except from the terrain generator's point of view: oasis can appear
                  | Forest
                  | DeepForest
                  | Water
                  | DeepWater
                  | Ice
                  | Lava
                  | Glass -- what sand becomes when struck by intense heat
                  | RecreantFactory
                  | Upstairs
                  | Downstairs
                    deriving (Read,Show,Eq,Ord)

data TerrainGenerationData = TerrainGenerationData
                           { tg_smootheness :: Integer,
                             tg_biome :: Biome,
                             tg_placements :: [TerrainPlacement] }
                           deriving (Read,Show)

data TerrainPlacement = TerrainPlacement {
    placement_sources :: [(Double,TerrainPatch)],
    placement_replacements :: [(Integer,TerrainPatch)],
    placement_seed :: Integer,
    placement_blob :: Blob }
        deriving (Read,Show)

placeTerrain :: TerrainPlacement -> TerrainGrid -> TerrainGrid
placeTerrain terrain_placement =
    arbitraryReplaceGrid (placement_sources terrain_placement)
                         (placement_replacements terrain_placement)
                         (placement_seed terrain_placement)
                         (placement_blob terrain_placement)

recreantFactories :: Integer -> TerrainPlacement
recreantFactories seed = TerrainPlacement {
    placement_sources =
        [(1/2,Ice),
         (1/10,Sand),
         (1/2,Desert),
         (1/5,Dirt),
         (1/1,Glass),
         (1/20,Grass),
         (1/100,Forest),
         (1/2,RockyGround)],
    placement_replacements =
        [(1,RecreantFactory)],
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
        [(1,Upstairs)],
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
        [(1,Downstairs)],
    placement_seed = seed,
    placement_blob = UnitBlob }

-- |
-- A list of TerrainPatches that are considered "difficult", either for traveling
-- or for constructing buildings.
--
difficult_terrains :: [TerrainPatch]
difficult_terrains = impassable_terrains ++
                     [Water,DeepWater,Ice,Lava]

-- |
-- A list of TerrainPatches that are considered "impassable" for traveling.
--
impassable_terrains :: [TerrainPatch]
impassable_terrains = [RockFace,Forest,DeepForest]

terrainFrequencies :: Biome -> [(Integer,TerrainPatch)]
terrainFrequencies ShallowDungeon = [(40,RockFace),(50,RockyGround),(5,Sand),(5,Dirt)]
terrainFrequencies DeepDungeon = [(50,RockFace),(25,Rubble),(25,RockyGround)]
terrainFrequencies FrozenDungeon = [(75,RockFace),(5,Rubble),(10,RockyGround),(10,Ice)]
terrainFrequencies AbyssalDungeon = [(60,RockFace),(10,Rubble),(10,RockyGround),(20,Water)]
terrainFrequencies InfernalDungeon = [(70,RockFace),(15,Rubble),(15,Lava)]
terrainFrequencies RockBiome = [(15,RockFace),(15,Rubble),(55,RockyGround),(15,Sand)]
terrainFrequencies IcyRockBiome = [(10,RockFace),(10,Rubble),(20,RockyGround),(60,Ice)]
terrainFrequencies GrasslandBiome = [(5,RockFace),(5,RockyGround),(10,Dirt),(10,Sand),(10,Forest),(10,Water),(50,Grass)]
terrainFrequencies ForestBiome = [(10,RockFace),(10,RockyGround),(10,Dirt),(10,Water),(10,Grass),(25,Forest),(25,DeepForest)]
terrainFrequencies TundraBiome = [(10,RockFace),(10,RockyGround),(10,Sand),(10,Water),(60,Ice)]
terrainFrequencies DesertBiome = [(10,RockFace),(10,RockyGround),(9,Grass),(1,Water),(70,Desert)]
terrainFrequencies OceanBiome = [(5,RockyGround),(10,Sand),(5,Grass),(5,Forest),(25,Water),(50,DeepWater)]
terrainFrequencies MountainBiome = [(50,RockFace),(25,RockyGround),(5,Rubble),(5,Sand),(5,Grass),(5,Forest),(5,Water)]
terrainFrequencies SwampBiome = [(40,Forest),(50,Water),(5,Sand),(5,Grass)]
terrainFrequencies PolarBiome = [(40,Ice),(30,Water),(5,DeepWater),(4,RockyGround),(1,RockFace)]

terrainInterpFn :: (TerrainPatch,TerrainPatch) -> [(Integer,TerrainPatch)]
terrainInterpFn (a,b) = [(1,a),(1,b)] ++ (terrainInterpRule (a,b)) ++ (terrainInterpRule (b,a))

-- Notice, in terrainInterpFn, we always throw in both terrain patches with a weight of 1.
terrainInterpRule :: (TerrainPatch,TerrainPatch) -> [(Integer,TerrainPatch)]
terrainInterpRule (RockFace,RockFace) = []
terrainInterpRule (RockFace,RockyGround) = [(3,RockFace),(1,Rubble),(3,RockyGround)]
terrainInterpRule (RockFace,x) = [(3,RockFace),(2,Rubble),(1,RockyGround),(1,Sand),(7,x)]
terrainInterpRule (Rubble,x) = [(1,Rubble),(2,Sand),(2,Dirt),(5,x)]
terrainInterpRule (DeepWater,DeepWater) = []
terrainInterpRule (DeepWater,Water) = [(3,DeepWater)]
terrainInterpRule (DeepWater,_) = [(3,Water)]
terrainInterpRule (DeepForest,DeepForest) = [(1,Grass)]
terrainInterpRule (DeepForest,Forest) = [(2,Grass)]
terrainInterpRule (DeepForest,_) = [(1,Forest)]
terrainInterpRule (Forest,DeepForest) = []
terrainInterpRule (Forest,Forest) = [(3,Grass)]
terrainInterpRule (Forest,_) = [(3,Grass)]
terrainInterpRule (Water,Water) = [(20,Water),(1,Sand)]
terrainInterpRule (Water,DeepWater) = []
terrainInterpRule (Water,_) = [(1,Sand)]
terrainInterpRule (Sand,Desert) = [(1,Grass),(1,Forest)]
terrainInterpRule _ = []

-- |
-- A list of every TerrainPatch that might be created from the terrainFrequencies function.
--
baseTerrainPatches :: [TerrainPatch]
baseTerrainPatches = nub $ List.map snd $ concatMap terrainFrequencies [minBound..maxBound]

terrainInterpMap :: Map (TerrainPatch,TerrainPatch) [(Integer,TerrainPatch)]
terrainInterpMap = let terrain_patch_pairs = [(a,b) | a <- baseTerrainPatches, b <- baseTerrainPatches]
                       interps = List.map terrainInterpFn terrain_patch_pairs
                       in fromList (zip terrain_patch_pairs interps)

type TerrainGrid = Grid TerrainPatch

-- |
-- Generates a random terrain map.  The Biome indicates determines what TerrainPatches
-- are generated.  The second parameter is an Integer that indicates the smootheness of the
-- generated terrain.  Finally, a random Integer stream is needed to provide the random data 
-- to generate the terrain.
--
generateTerrain :: TerrainGenerationData -> [Integer] -> TerrainGrid
generateTerrain tg rands = flip (List.foldr placeTerrain) (tg_placements tg) $
    generateGrid (terrainFrequencies (tg_biome tg))
                 terrainInterpMap
                 (tg_smootheness tg)
                 rands

