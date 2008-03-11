
module TerrainData
    (Biome(..),
     TerrainPatch(..),
     TerrainMap,
     TerrainGenerationData(..),
     generateTerrain,
     generateExampleTerrain,
     prettyPrintTerrain,
     difficult_terrains)
    where

import Grids
import Data.List as List
import Data.Map as Map
import Substances
import RNG

-- |
-- Most automatically generated surface maps belong to a Biome, representing the kind of terrain
-- and plant life that dwells in terrain generated for the map.
--
data Biome = RockBiome
           | IcyRockBiome
           | GrasslandBiome
	   | ForestBiome
           | TundraBiome
           | DesertBiome
           | OceanBiome
           | MountainBiome
	   | SwampBiome
	     deriving (Read,Show,Eq,Ord,Enum,Bounded)

-- |
-- All static terrain elements are members of TerrainMap
--
-- The only difference between "Deasert" and "Sand" is that where
-- "Deasert" and "Water" touch, the map generator will produce
-- patches of plantlife (for oasis and shoreline effect).
--
data TerrainPatch = RockFace
                  | Rubble
                  | Ore Solid
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
                    deriving (Read,Show,Eq,Ord)

data TerrainGenerationData = TerrainGenerationData
			   { tg_smootheness :: Integer,
			     tg_biome :: Biome }
			   deriving (Read,Show)

-- |
-- A list of TerrainPatches that are considered "difficult", either for traveling
-- or for constructing buildings.
--
difficult_terrains :: [TerrainPatch]
difficult_terrains = [RockFace,Forest,DeepForest,Water,DeepWater,Ice,Lava]

terrainFrequencies :: Biome -> [(Integer,TerrainPatch)]
terrainFrequencies RockBiome = [(1,RockFace),(1,Rubble),(3,RockyGround),(1,Sand)]
terrainFrequencies IcyRockBiome = [(1,RockFace),(2,Rubble),(3,RockyGround),(6,Ice)]
terrainFrequencies GrasslandBiome = [(1,RockFace),(1,RockyGround),(1,Dirt),(2,Sand),(1,Forest),(1,Water),(10,Grass)]
terrainFrequencies ForestBiome = [(1,RockFace),(1,RockyGround),(1,Dirt),(5,Water),(3,Grass),(5,Forest),(5,DeepForest)]
terrainFrequencies TundraBiome = [(1,RockFace),(3,RockyGround),(1,Sand),(1,Water),(1,Grass),(8,Ice)]
terrainFrequencies DesertBiome = [(1,RockFace),(3,RockyGround),(1,Grass),(1,Water),(11,Desert)]
terrainFrequencies OceanBiome = [(1,RockyGround),(3,Sand),(1,Grass),(1,Forest),(7,Water),(20,DeepWater)]
terrainFrequencies MountainBiome = [(6,RockFace),(3,RockyGround),(1,Rubble),(1,Sand),(1,Grass),(1,Forest),(1,Water)]
terrainFrequencies SwampBiome = [(1,Forest),(1,Water)]

terrainInterpFn :: (TerrainPatch,TerrainPatch) -> [(Integer,TerrainPatch)]
terrainInterpFn (a,b) = [(1,a),(1,b)] ++ (terrainInterpRule (a,b)) ++ (terrainInterpRule (b,a))

terrainInterpRule :: (TerrainPatch,TerrainPatch) -> [(Integer,TerrainPatch)]
terrainInterpRule (RockFace,RockFace) = []
terrainInterpRule (RockFace,RockyGround) = [(3,RockFace),(1,Rubble),(3,RockyGround)]
terrainInterpRule (RockFace,x) = [(3,RockFace),(2,Rubble),(1,RockyGround),(1,Sand),(7,x)]
terrainInterpRule (Rubble,x) = [(1,Rubble),(2,Sand),(2,Dirt),(5,x)]
terrainInterpRule (DeepWater,DeepWater) = []
terrainInterpRule (DeepWater,Water) = [(3,DeepWater)]
terrainInterpRule (DeepWater,_) = [(3,Water)]
terrainInterpRule (DeepForest,DeepForest) = []
terrainInterpRule (DeepForest,Forest) = [(3,DeepForest)]
terrainInterpRule (DeepForest,_) = [(5,Forest)]
terrainInterpRule (Forest,DeepForest) = []
terrainInterpRule (Forest,Forest) = []
terrainInterpRule (Forest,_) = [(1,Grass)]
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

type TerrainMap = Grid TerrainPatch

-- |
-- Generates a random terrain map.  The Biome indicates determines what TerrainPatches
-- are generated.  The second parameter is an Integer that indicates the smootheness of the
-- generated terrain.  Finally, a random Integer stream is needed to provide the random data 
-- to generate the terrain.
--
generateTerrain :: TerrainGenerationData -> [Integer] -> TerrainMap
generateTerrain tg rands = 
    generateGrid (terrainFrequencies (tg_biome tg))
		 terrainInterpMap
		 (tg_smootheness tg)
		 rands

terrainPatchToASCII :: TerrainPatch -> Char
terrainPatchToASCII RockFace = '#'
terrainPatchToASCII Rubble = '*'
terrainPatchToASCII (Ore _) = '$'
terrainPatchToASCII RockyGround = ':'
terrainPatchToASCII Dirt = '.'
terrainPatchToASCII Grass = ','
terrainPatchToASCII Sand = '_'
terrainPatchToASCII Desert = '_'
terrainPatchToASCII Forest = 'f'
terrainPatchToASCII DeepForest = 'F'
terrainPatchToASCII Water = '~'
terrainPatchToASCII DeepWater = '~'
terrainPatchToASCII Ice = '^'
terrainPatchToASCII Glass = '_'
terrainPatchToASCII Lava = '^'

exampleTerrainGenerator :: TerrainGenerationData
exampleTerrainGenerator = TerrainGenerationData
			  { tg_smootheness = 5,
			    tg_biome = ForestBiome }

generateExampleTerrain :: Integer -> TerrainMap
generateExampleTerrain seed = generateTerrain exampleTerrainGenerator (randomIntegerStream seed)

prettyPrintTerrain :: ((Integer,Integer),(Integer,Integer)) -> TerrainMap -> [String]
prettyPrintTerrain ((left_bound,right_bound),(top_bound,bottom_bound)) terrain_map =
    [[terrainPatchToASCII $ gridAt terrain_map (x,y) 
      | x <- [left_bound..right_bound]]
     | y <- [top_bound..bottom_bound]]