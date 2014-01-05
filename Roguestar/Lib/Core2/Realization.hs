{-# LANGUAGE ScopedTypeVariables #-}
module Roguestar.Lib.Core2.Realization
    (realizePlane,
     realizeMonster,
     realizeSquare)
    where

--
-- This module extracts information from the database and builds a navigable object graph.
--
-- See Roguestar.Lib.DB to see the database implementation we are pulling from.
-- See Roguestar.Lib.Graph to see the graph model we are realizing.
--

import Prelude hiding (getContents)
import qualified Roguestar.Lib.Data.PlaneData as PlaneData
import Roguestar.Lib.DB
import Roguestar.Lib.Graph
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Set as Set

realizePlane :: DB_BaseType -> PlaneRef -> Plane
realizePlane db plane_ref = Plane {
    plane_to_reference = plane_ref,
    plane_to_data = getPlane plane_ref db,
    plane_to_monsters = Set.fromList $ map (realizeMonster db . asChild) $ mapMaybe fromLocation $ getContents plane_ref db,
    plane_to_buildings = Set.empty }

realizeMonster :: DB_BaseType -> MonsterRef -> Monster
realizeMonster db monster_ref = Monster {
    monster_to_reference = monster_ref,
    monster_to_data = getMonster monster_ref db,
    monster_to_square = realizeSquare db plane_ref p }
        where (p :: Position, Parent plane_ref :: Parent PlaneData.Plane) = fromMaybe (error "realizeMonster: doesn't have a planar position") $ fromLocation $ whereIs monster_ref db

realizeSquare :: DB_BaseType -> PlaneRef -> Position -> Square
realizeSquare db plane_ref p = Square {
     square_to_plane = realizePlane db plane_ref,
     square_to_position = p }

