module Roguestar.Lib.Graph.TestExampleEntities
    (equestria, picard, twilight, zathras)
    where

import qualified Roguestar.Lib.Data.ReferenceTypes as References
import Roguestar.Lib.Graph.Graph
import qualified Data.Set as Set

equestria :: Plane
equestria = Plane {
              plane_to_reference = References.PlaneRef 0,
              plane_to_data = error "undefined equestria",
              plane_to_monsters = Set.fromList [twilight, picard, zathras],
              plane_to_buildings = Set.fromList [] }

twilight :: Monster
twilight = Monster {
              monster_to_data = error "undefined twilight",
              monster_to_reference = References.MonsterRef 1,
              monster_to_square = Square equestria (error "No Position") }

picard :: Monster
picard = Monster {
              monster_to_data = error "undefined picard",
              monster_to_reference = References.MonsterRef 2,
              monster_to_square = Square equestria (error "No Position") }

zathras :: Monster
zathras = Monster {
              monster_to_data = error "undefined zathras",
              monster_to_reference = References.MonsterRef 3,
              monster_to_square = Square equestria (error "No Position") }
