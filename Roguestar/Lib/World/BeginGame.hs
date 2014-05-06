{-# LANGUAGE OverloadedStrings #-}
module Roguestar.Lib.World.BeginGame
    (beginGame)
    where

-- World
import Roguestar.Lib.Core.Plane
import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.Data.BuildingData
import Roguestar.Lib.DB
import Roguestar.Lib.Data.FacingData
import Roguestar.Lib.Data.TerrainData
import Roguestar.Lib.Data.ToolData
import Control.Monad
import Control.Monad.Error
import Roguestar.Lib.Data.SpeciesData
import Roguestar.Lib.Data.PlayerState
import Roguestar.Lib.Town
import qualified Data.ByteString.Char8 as B ()
import Control.Monad.Random
import Roguestar.Lib.Utility.SiteCriteria

homeBiome :: Species -> WeightedSet Biome
homeBiome RedRecreant = weightedSet [(2,TemperateForest),(2,TemperateClearing),(1,RelaxingPond),(1,CraterInterior)]
homeBiome LavaLarva = weightedSet [(1,RelaxingPond),(2,LavaPool),(3,CraterInterior)]
homeBiome Anachronid = weightedSet [(5,TropicalForest),(1,TropicalClearing)]
homeBiome TabularMonstrosity = weightedSet [(3,BareMountain),(1,CraterInterior)]

startingEquipmentBySpecies :: Species -> [Tool]
startingEquipmentBySpecies RedRecreant = []
startingEquipmentBySpecies LavaLarva = []
startingEquipmentBySpecies Anachronid = []
startingEquipmentBySpecies TabularMonstrosity = []

dbCreateStartingPlane :: MonsterData -> DB PlaneRef
dbCreateStartingPlane creature =
    do seed <- getRandom
       dbNewPlane "belhaven" (TerrainGenerationData {
           tg_smootheness = 3,
           tg_biome = homeBiome (creature_species creature),
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
       landing_site <- pickRandomSite (-150,150) (-150,150) 150 [areaClearForObjectPlacement 0, atDistanceFrom (Position (0,0)) 100] plane_ref
       creature_ref <- dbAddMonster creature (Standing plane_ref landing_site Here)
       setPlayerMonster creature_ref
       _ <- createTown plane_ref [basic_stargate]
       let starting_equip = startingEquipmentBySpecies (creature_species creature)
       forM_ starting_equip $ \tool -> dbAddTool tool (Inventory creature_ref)
       -- (_,end_of_nonaligned_first_series) <- makePlanets (Subsequent plane_ref NonAlignedRegion) =<< generatePlanetInfo nonaligned_first_series_planets
       -- _ <- makePlanets (Subsequent end_of_nonaligned_first_series NonAlignedRegion) =<< generatePlanetInfo nonaligned_second_series_planets
       -- _ <- makePlanets (Subsequent end_of_nonaligned_first_series CyborgRegion) =<< generatePlanetInfo cyborg_planets
       setPlayerState $ PlayerMonsterTurn creature_ref

