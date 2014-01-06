module Roguestar.Lib.Graph.Graph
    (Monster(..),
     Plane(..),
     Square(..),
     Building(..))
    where

import qualified Data.Set as Set
import qualified Roguestar.Lib.Data.ReferenceTypes as References
import qualified Roguestar.Lib.Data.MonsterData as MonsterData
import qualified Roguestar.Lib.Data.PlaneData as PlaneData
import Roguestar.Lib.Position

data Monster = Monster {
    monster_to_reference :: References.MonsterRef,
    monster_to_data :: MonsterData.MonsterData,
    monster_to_square :: Square }

data Square = Square {
    square_to_plane :: Plane,
    square_to_position :: Position }

data Plane = Plane {
    plane_to_reference :: References.PlaneRef,
    plane_to_data :: PlaneData.Plane,
    plane_to_monsters :: Set.Set Monster,
    plane_to_buildings :: Set.Set Building }

data Building = Building {
    building_to_reference :: References.BuildingRef,
    building_to_position :: Set.Set Square }

instance Eq Monster where
    a == b = monster_to_reference a == monster_to_reference b

instance Eq Plane where
    a == b = plane_to_reference a == plane_to_reference b

instance Eq Building where
    a == b = building_to_reference a == building_to_reference b

instance Ord Monster where
    compare a b = compare (monster_to_reference a) (monster_to_reference b)

instance Ord Plane where
    compare a b = compare (plane_to_reference a) (plane_to_reference b)

instance Ord Building where
    compare a b = compare (building_to_reference a) (building_to_reference b)

instance Show Monster where
    show = show . monster_to_reference

instance Show Plane where
    show = show . plane_to_reference

instance Show Building where
    show = show . building_to_reference

