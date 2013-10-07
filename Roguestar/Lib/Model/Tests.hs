module Roguestar.Lib.Model.Tests
    (testcases)
    where

import Roguestar.Lib.Model.Graph
import Roguestar.Lib.Model.Classes
import qualified Data.Set as Set
import Test.HUnit

testcases :: Test
testcases = TestLabel "Roguestar.Lib.Model.Tests" $ TestList [
    testPlaneToSelf,
    testMonsterToPlane,
    testCoMonsters]

data ID =
    Equestria
  | Nirn
  | Twilight
  | Ysolda
  | Zathras
    deriving (Eq, Ord, Show)

equestria :: Plane ID
equestria = Plane {
              plane_to_uid = Equestria,
              plane_to_monsters = Set.fromList [twilight, ysolda, zathras] }

twilight :: Monster ID
twilight = Monster {
              monster_to_uid = Twilight,
              monster_to_square = Square equestria }

ysolda :: Monster ID
ysolda = Monster {
              monster_to_uid = Ysolda,
              monster_to_square = Square equestria }

zathras :: Monster ID
zathras = Monster {
              monster_to_uid = Zathras,
              monster_to_square = Square equestria }

testPlaneToSelf :: Test
testPlaneToSelf = TestCase $ assertEqual "testPlaneToSelf" equestria (plane equestria)

testMonsterToPlane :: Test
testMonsterToPlane = TestCase $ assertEqual "testMonsterToPlane" equestria (plane ysolda)

testCoMonsters :: Test
testCoMonsters = TestCase $ assertEqual "testCoMonsters" (Set.fromList [twilight, ysolda]) (comonsters zathras)

