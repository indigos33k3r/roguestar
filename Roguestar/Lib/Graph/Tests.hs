module Roguestar.Lib.Graph.Tests
    (testcases,
     equestria, picard, twilight, zathras)
    where

import qualified Roguestar.Lib.Data.ReferenceTypes as References
import Roguestar.Lib.Graph.Graph
import Roguestar.Lib.Graph.Classes
import Roguestar.Lib.Graph.TestExampleEntities
import qualified Data.Set as Set
import Test.HUnit

testcases :: Test
testcases = TestLabel "Roguestar.Lib.Model.Tests" $ TestList [
    testPlaneToSelf,
    testMonsterToPlane]

testPlaneToSelf :: Test
testPlaneToSelf = TestCase $ assertEqual "testPlaneToSelf" equestria (plane equestria)

testMonsterToPlane :: Test
testMonsterToPlane = TestCase $ assertEqual "testMonsterToPlane" equestria (plane picard)
