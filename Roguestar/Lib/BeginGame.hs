{-# LANGUAGE OverloadedStrings #-}
module Roguestar.Lib.BeginGame
    (beginGame)
    where

import Roguestar.Lib.Plane
import Roguestar.Lib.CreatureData
import Roguestar.Lib.Character
import Roguestar.Lib.BuildingData
import Roguestar.Lib.DB
import Roguestar.Lib.Facing
import Roguestar.Lib.TerrainData
import Roguestar.Lib.ToolData
import Control.Monad
import Control.Monad.Error
import Roguestar.Lib.SpeciesData
import Roguestar.Lib.Substances as Substances
import Roguestar.Lib.PlayerState
import Roguestar.Lib.Town
import Roguestar.Lib.PlanetData
import Roguestar.Lib.Planet
import qualified Data.ByteString.Char8 as B ()
import Control.Monad.Random

homeBiome :: Species -> [Biome]
homeBiome RedRecreant = [ForestBiome,TundraBiome,MountainBiome]
homeBiome BlueRecreant = [ForestBiome,TundraBiome,MountainBiome]

startingEquipmentBySpecies :: Species -> [Tool]
startingEquipmentBySpecies RedRecreant = []
startingEquipmentBySpecies BlueRecreant = []

dbCreateStartingPlane :: Creature -> DB PlaneRef
dbCreateStartingPlane creature =
    do seed <- getRandom
       biome <- pickM $ homeBiome (creature_species creature)
       dbNewPlane "belhaven" (TerrainGenerationData {
           tg_smootheness = 2,
           tg_biome = biome,
           tg_placements = [recreantFactories seed] }) TheUniverse

-- |
-- Begins the game with the specified starting player creature.
--
beginGame :: DB ()
beginGame =
    do player_state <- playerState
       creature <- case player_state of
           SpeciesSelectionState (Just c) -> return c
           _ -> throwError $ DBError "Tried to begin a game, but no species/creature has been selected."
       plane_ref <- dbCreateStartingPlane creature
       landing_site <- pickRandomClearSite 200 30 2 (Position (0,0)) (not . (`elem` difficult_terrains)) plane_ref
       creature_ref <- dbAddCreature creature (Standing plane_ref landing_site Here)
       setPlayerCreature creature_ref
       _ <- createTown plane_ref [basic_stargate]
       let starting_equip = startingEquipmentBySpecies (creature_species creature)
       forM_ starting_equip $ \tool -> dbAddTool tool (Inventory creature_ref)
       -- (_,end_of_nonaligned_first_series) <- makePlanets (Subsequent plane_ref NonAlignedRegion) =<< generatePlanetInfo nonaligned_first_series_planets
       -- _ <- makePlanets (Subsequent end_of_nonaligned_first_series NonAlignedRegion) =<< generatePlanetInfo nonaligned_second_series_planets
       -- _ <- makePlanets (Subsequent end_of_nonaligned_first_series CyborgRegion) =<< generatePlanetInfo cyborg_planets
       setPlayerState $ PlayerCreatureTurn creature_ref

