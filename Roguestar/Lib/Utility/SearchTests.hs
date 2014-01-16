module Roguestar.Lib.Utility.SearchTests
    (testcases)
    where

import Roguestar.Lib.Utility.Search
import qualified Test.HUnit as HUnit
import qualified Data.Map as Map

testcases :: HUnit.Test
testcases = HUnit.TestList [testSimpleShortestPath,
                            testFloodFillWithinBounds,
                            testFloodFillOutOfBounds]

expected :: Maybe (Path (Integer,Integer))
expected = Just (5,[(5,5),(4,4),(3,3),(2,2),(1,1),(0,0)])

testSimpleShortestPath :: HUnit.Test
testSimpleShortestPath = HUnit.TestCase $ HUnit.assertEqual "testSimpleShortestPath" expected $ Map.lookup (5,5) result
    where result = search (searchableChessboard (5,5)) (startingAt (0,0))

example_chessboard_flood_fill :: Paths (Integer,Integer)
example_chessboard_flood_fill = search (floodfillChessboard $ \(x,y) -> abs x < 10 && abs y < 10) (startingAt (0,0))

testFloodFillWithinBounds :: HUnit.Test
testFloodFillWithinBounds = HUnit.TestCase $ HUnit.assertEqual "testFloodFillWithinBounds" expected $ Map.lookup (5,5) example_chessboard_flood_fill

testFloodFillOutOfBounds :: HUnit.Test
testFloodFillOutOfBounds = HUnit.TestCase $ HUnit.assertEqual "testFloodFillOutOfBounds" Nothing $ Map.lookup (12,12) example_chessboard_flood_fill
