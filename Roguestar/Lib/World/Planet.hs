{-# LANGUAGE TypeFamilies #-}
--World
module Roguestar.Lib.World.Planet
    (makePlanets,
     generatePlanetInfo)
    where

import Roguestar.Lib.Data.PlanetData
import Roguestar.Lib.Data.PlaneData
import Roguestar.Lib.DB
import Roguestar.Lib.Core.Plane
import Roguestar.Lib.Data.TerrainData
import Control.Monad
import Control.Monad.Random
import Data.Maybe
import Data.Ord
import Roguestar.Lib.Town
import Data.List
import Data.ByteString.Char8 as B
import Roguestar.Lib.Data.FactionData
import Roguestar.Lib.Data.BuildingData

makePlanet :: (LocationConstructor l, ChildTypeOf l ~ PlaneData) => l -> PlanetInfo -> DB PlaneRef
makePlanet plane_location planet_info =
    do seed <- getRandom
       seed_down <- getRandom
       planet_name <- liftM (`fromMaybe` planet_info_name planet_info) $
                          randomPlanetName Nonaligned
       plane_ref <- dbNewPlane
          planet_name
          (TerrainGenerationData {
               tg_smootheness = 3,
               tg_biome = planet_info_biome planet_info,
               tg_placements = [recreantFactories seed,
                                stairsDown seed_down 0] })
          plane_location
       town <- liftM catMaybes $ forM (planet_info_town planet_info) $ \(r,b) ->
           do p <- rationalRoll r
              return $ if p then Just b else Nothing
       _ <- createTown plane_ref town
       _ <- makeDungeons planet_name (Beneath plane_ref) 1 planet_info
       return plane_ref

makePlanets :: Subsequent -> [PlanetInfo]  -> DB (PlaneRef,PlaneRef)
makePlanets _ [] = return $ error "makePlanets: empty list"
makePlanets l (planet_info:[]) =
    do plane_ref <- makePlanet l planet_info
       return (plane_ref,plane_ref)
makePlanets l (planet_info:rest) =
    do first_plane_ref <- makePlanet l planet_info
       (_,last_plane_ref) <- makePlanets (Subsequent first_plane_ref $ subsequent_via l) rest
       return (first_plane_ref,last_plane_ref)

makeDungeons :: (LocationConstructor l, ChildTypeOf l ~ PlaneData) =>
                B.ByteString ->
                l ->
                Integer ->
                PlanetInfo ->
                DB PlaneRef
makeDungeons planet_name plane_location i planet_info =
    do let n = planet_info_depth planet_info
       seed_up <- getRandom
       seed_down <- getRandom
       plane_ref <- dbNewPlane
           planet_name
           (TerrainGenerationData {
               tg_smootheness = 2,
               tg_biome = planet_info_dungeon planet_info,
               tg_placements =
                   [stairsUp seed_up i] ++
                   if i < n then [stairsDown seed_down i] else [] })
           plane_location
       when (i == n) $
           do _ <- createTown plane_ref $ Data.List.replicate (fromInteger n) powerup
              return ()
       when (i < n) $
           do _ <- makeDungeons planet_name (Beneath plane_ref) (succ i) planet_info
              return ()
       return plane_ref

generatePlanetInfo :: (DBReadable db) => [PlanetInfo] -> db [PlanetInfo]
generatePlanetInfo planet_infos = liftM (sortBy (comparing planet_info_priority)) $ forM planet_infos $ \planet_info ->
    do -- see documentation for 'PlanetData.PlanetInfo'
       prio_bonus <- getRandomR (0.0,1.0)
       return $ planet_info { planet_info_priority =
           planet_info_priority planet_info + prio_bonus }

