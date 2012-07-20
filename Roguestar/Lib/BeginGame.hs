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

homeBiome :: Species -> Biome
homeBiome Anachronid = ForestBiome
homeBiome Ascendant = MountainBiome
homeBiome Androsynth = IcyRockBiome
homeBiome Caduceator = GrasslandBiome
homeBiome Encephalon = SwampBiome
homeBiome Goliath = DesertBiome
homeBiome Hellion = SwampBiome
homeBiome Kraken = OceanBiome
homeBiome Myrmidon = DesertBiome
homeBiome Perennial = GrasslandBiome
homeBiome Recreant = TundraBiome
homeBiome Reptilian = ForestBiome
homeBiome DustVortex = DesertBiome

startingEquipmentBySpecies :: Species -> [Tool]
startingEquipmentBySpecies Anachronid = [sphere Radon]
startingEquipmentBySpecies Ascendant = [sphere Neon]
startingEquipmentBySpecies Androsynth = [sphere Silicon]
startingEquipmentBySpecies Caduceator = [sphere Silver]
startingEquipmentBySpecies Encephalon = [sphere Ammonia]
startingEquipmentBySpecies Goliath = [sphere Iron]
startingEquipmentBySpecies Hellion = [sphere Methane]
startingEquipmentBySpecies Kraken = [sphere Substances.Water]
startingEquipmentBySpecies Myrmidon = [sphere Krypton]
startingEquipmentBySpecies Perennial = [sphere Wood]
startingEquipmentBySpecies Recreant = [sphere Malignite]
startingEquipmentBySpecies Reptilian = [sphere Oxygen]
startingEquipmentBySpecies DustVortex = [sphere Aluminum, sphere Nitrogen]

dbCreateStartingPlane :: Creature -> DB PlaneRef
dbCreateStartingPlane creature =
    do dbNewPlane "belhaven" (TerrainGenerationData {
           tg_smootheness = 3,
           tg_biome = homeBiome $ creature_species creature,
           tg_placements = [] }) TheUniverse

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
       _ <- createTown plane_ref [basic_stargate,monolith]
       let starting_equip = startingEquipmentBySpecies (creature_species creature)
       forM_ starting_equip $ \tool -> dbAddTool tool (Inventory creature_ref)
       forM_ [0..10] $ \_ -> do tool_position <- pickRandomClearSite 200 1 2 landing_site (not . (`elem` difficult_terrains)) plane_ref
                                tool_type <- weightedPickM [(8,phase_pistol),(5,phaser),(3,phase_rifle),(8,kinetic_fleuret),(3,kinetic_sabre),
                                                              (5,Sphere $ toSubstance Nitrogen),(5,Sphere $ toSubstance Ionidium),(5,Sphere $ toSubstance Aluminum)]
                                dbAddTool tool_type (Dropped plane_ref tool_position)
       (_,end_of_nonaligned_first_series) <- makePlanets (Subsequent plane_ref NonAlignedRegion) =<< generatePlanetInfo nonaligned_first_series_planets
       _ <- makePlanets (Subsequent end_of_nonaligned_first_series NonAlignedRegion) =<< generatePlanetInfo nonaligned_second_series_planets
       _ <- makePlanets (Subsequent end_of_nonaligned_first_series CyborgRegion) =<< generatePlanetInfo cyborg_planets
       setPlayerState $ PlayerCreatureTurn creature_ref

