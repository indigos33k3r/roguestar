module Roguestar.Lib.Core2.Tests 
    (testcases)
    where

import Test.HUnit
import Roguestar.Lib.Graph.TestExampleEntities
import Roguestar.Lib.Core2.Monster
import qualified Data.Set as Set

testcases :: Test
testcases = TestList $ [testCoMonsters]

testCoMonsters :: Test
testCoMonsters = TestCase $ assertEqual "testCoMonsters" (Set.fromList [twilight, picard]) (comonsters zathras)
