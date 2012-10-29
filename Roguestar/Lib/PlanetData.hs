{-# LANGUAGE OverloadedStrings #-}
--World
module Roguestar.Lib.PlanetData
    (PlanetRegion(..),
     PlanetInfo(..),
     addTown,
     nonaligned_first_series_planets,
     nonaligned_second_series_planets,
     cyborg_planets)
    where

import Roguestar.Lib.PersistantData
import Roguestar.Lib.TerrainData
import Roguestar.Lib.BuildingData
import Data.Ratio
import qualified Data.ByteString.Char8 as B

-- | Information used to construct new planets.  A "planet" is an abstract stack of
-- Planes, with the planet's surface being on top and various dungeon levels below.
--
-- 'PlanetInfo's are sorted by their 'planet_info_priority' fields.
--
data PlanetInfo = PlanetInfo {
    -- | Between 0 and 1 are randomly added to this value, and then all 'PlanetInfo's are sorted by priority.
    -- This gives the order in which players visit planets.
    planet_info_priority :: Double,
    planet_info_region :: PlanetRegion,
    -- | Some planets have names.
    planet_info_name :: Maybe B.ByteString,
    -- | Number of dungeon levels on the planet.
    planet_info_depth :: Integer,
    planet_info_biome :: Biome,
    planet_info_dungeon :: Biome,
    planet_info_town :: [(Rational,BuildingPrototype)],
    planet_info_node_type :: BuildingPrototype }

nonaligned :: Integer -> B.ByteString -> Biome -> PlanetInfo
nonaligned x name biome = PlanetInfo {
    planet_info_priority = fromInteger x / 3,
    planet_info_region = NonAlignedRegion,
    planet_info_name = case name of
                           "" -> Nothing
                           _  -> Just name,
    planet_info_depth = x,
    planet_info_biome = biome,
    planet_info_dungeon = case () of
        () | biome == OceanBiome -> AbyssalDungeon
        () | biome == SwampBiome -> AbyssalDungeon
        () | x == 1 -> ShallowDungeon
        () -> DeepDungeon,
    planet_info_town = [(1,basic_stargate)],
    planet_info_node_type = powerup }

cyber :: B.ByteString -> Biome -> PlanetInfo
cyber name biome = PlanetInfo {
    planet_info_priority = 0.0,
    planet_info_region = CyborgRegion,
    planet_info_name = case name of
                           "" -> Nothing
                           _ -> Just name,
    planet_info_depth = 5,
    planet_info_biome = biome,
    planet_info_dungeon = FrozenDungeon,
    planet_info_town = [(1,cybergate)],
    planet_info_node_type = powerup }

addTown :: PlanetInfo -> [(Rational,BuildingPrototype)] -> PlanetInfo
addTown planet_info town = planet_info { planet_info_town = planet_info_town planet_info ++ town }

removeTown :: PlanetInfo -> [(BuildingPrototype)] -> PlanetInfo
removeTown planet_info town = planet_info { planet_info_town = filter (\(_,building) -> not $ building `elem` town) $ planet_info_town planet_info }

nonaligned_first_series_planets :: [PlanetInfo]
nonaligned_first_series_planets = [
    nonaligned 1 "" RockBiome,
    nonaligned 1 "" IcyRockBiome,
    nonaligned 2 "roanoke" SwampBiome,
    nonaligned 2 "pamlico" SwampBiome,
    nonaligned 2 "pungo" ForestBiome,
    nonaligned 2 "neuse" ForestBiome,
    nonaligned 2 "crabtree" SwampBiome,
    nonaligned 2 "eno" SwampBiome `addTown` [(1%20,monolith)],
    nonaligned 2 "yadkin" SwampBiome,
    nonaligned 2 "catawba" ForestBiome,
    (nonaligned 5 "pasquotank" ForestBiome `addTown` [(1,cybergate)]) { planet_info_priority = 100.0 }]

nonaligned_second_series_planets :: [PlanetInfo]
nonaligned_second_series_planets = [
    nonaligned 1 "" TundraBiome,
    nonaligned 1 "" DesertBiome,
    nonaligned 1 "" MountainBiome,
    nonaligned 2 "dogwood" GrasslandBiome,
    nonaligned 3 "cardinal" GrasslandBiome,
    nonaligned 4 "currituck" OceanBiome,
    nonaligned 4 "hatteras" OceanBiome,
    nonaligned 4 "lookout" OceanBiome,
    nonaligned 4 "ocracoke" OceanBiome,
    (nonaligned 7 "emerald" GrasslandBiome `removeTown` [basic_stargate]) { planet_info_priority = 100.0 }]

cyborg_planets :: [PlanetInfo]
cyborg_planets = [
    cyber "" TundraBiome,
    cyber "" TundraBiome,
    cyber "" TundraBiome,
    cyber "rainwater" PolarBiome,
    cyber "spyglass" PolarBiome,
    cyber "fairview" IcyRockBiome,
    cyber "iredale" IcyRockBiome,
    (cyber "belleview" IcyRockBiome `removeTown` [cybergate]) { planet_info_priority = 100.0 }]

