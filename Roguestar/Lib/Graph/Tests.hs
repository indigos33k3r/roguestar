module Roguestar.Lib.Graph.Tests
    (testcases)
    where

import qualified Roguestar.Lib.Data.ReferenceTypes as References
import Roguestar.Lib.Graph.Graph
import Roguestar.Lib.Graph.Classes
import qualified Data.Set as Set
import Test.HUnit

testcases :: Test
testcases = TestLabel "Roguestar.Lib.Model.Tests" $ TestList [
    testPlaneToSelf,
    testMonsterToPlane,
    testCoMonsters]

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

testPlaneToSelf :: Test
testPlaneToSelf = TestCase $ assertEqual "testPlaneToSelf" equestria (plane equestria)

testMonsterToPlane :: Test
testMonsterToPlane = TestCase $ assertEqual "testMonsterToPlane" equestria (plane picard)

testCoMonsters :: Test
testCoMonsters = TestCase $ assertEqual "testCoMonsters" (Set.fromList [twilight, picard]) (comonsters zathras)

