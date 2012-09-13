module Roguestar.Lib.Town
    (createTown)
    where

import Roguestar.Lib.BuildingData
import Roguestar.Lib.DB
import Roguestar.Lib.TerrainData
import Roguestar.Lib.Plane

-- | Create a town from a list of buildings.
createTown :: PlaneRef -> [BuildingPrototype] -> DB [BuildingRef]
createTown plane_ref = mapM $ \building_prototype ->
    do let clear_need = minimum $ map abs $ uncurry (++) $ unzip $ buildingOccupies $ buildingproto_shape building_prototype
       p <- pickRandomClearSite 1 (clear_need*2+1) (clear_need+1) (Position (0,0)) (not . (`elem` difficult_terrains)) plane_ref
       let the_building = Building {
                              building_behavior = buildingproto_behavior building_prototype,
                              building_signal = buildingproto_signal building_prototype }
       let the_location = Constructed {
           constructed_plane = plane_ref,
           constructed_position = p,
           constructed_shape = buildingproto_shape building_prototype }
       dbAddBuilding the_building the_location
