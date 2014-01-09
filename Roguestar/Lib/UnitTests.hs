{-# LANGUAGE OverloadedStrings #-}

module Roguestar.Lib.UnitTests (runTests) where

import Data.Text as T
import Control.Monad.Writer.Lazy as W
import Roguestar.Lib.Roguestar
import Data.Maybe
import Data.List as List
import Control.Concurrent
import Roguestar.Lib.DB
import Roguestar.Lib.Data.PlayerState
import Control.Monad.Reader.Class
import Roguestar.Lib.Core.Plane
import Roguestar.Lib.Data.TerrainData
import Roguestar.Lib.Utility.SiteCriteria
import Roguestar.Lib.Random as Random

import qualified Test.HUnit.Base as HUnit
import qualified Roguestar.Lib.Graph.Tests as GraphTests
import qualified Roguestar.Lib.Core.Tests as CoreTests
import qualified Roguestar.Lib.Core2.Tests as Core2Tests
import qualified Roguestar.Lib.Utility.HierarchicalDatabase as HDatabaseTests

runTests :: IO (T.Text,Bool)
runTests =
    do (counts, text) <- captureTestResults testcases
       return (text,  HUnit.errors counts == 0 && HUnit.failures counts == 0)

data TestResult = TestResult {
    test_result_text :: [T.Text] }

pathOf :: HUnit.State -> String
pathOf (HUnit.State { HUnit.path = p }) = List.concat $ List.map (nodeToString) $ List.reverse p
    where nodeToString (HUnit.ListItem i) = "[" ++ show i ++ "]"
          nodeToString (HUnit.Label s) = "/" ++ s

captureTestResults :: HUnit.Test -> IO (HUnit.Counts, T.Text)
captureTestResults test =
        do (counts, test_result) <- HUnit.performTest report_start report_problem report_problem (TestResult []) test
           return (counts, T.concat $ List.intersperse "\n\n" $ List.reverse $ test_result_text test_result)
    where report_start state test_result = return $ test_result { test_result_text = (T.pack $ "\n" ++ pathOf state) : test_result_text test_result }
          report_problem msg state test_result = return $ test_result { test_result_text = (T.pack $ pathOf state ++ ": " ++ msg) : test_result_text test_result }

-- Generate N random planes and run tests against them.
runWithRandomPlanes :: Int -> String -> (PlaneRef -> DB HUnit.Assertion) -> HUnit.Test
runWithRandomPlanes n test_name db_action = HUnit.TestList $ (flip Prelude.map) [1..n] $ \x -> HUnit.TestLabel (test_name ++ "#" ++ show x) $ HUnit.TestCase $
    liftIO $ do result <- runDB (runWithRandomPlane_ db_action) initial_db
                case result of
                    (Right (assertion, _)) -> assertion
                    (Left (err)) -> HUnit.assertString (show err)

runWithRandomPlane_ :: (PlaneRef -> DB HUnit.Assertion) -> DB HUnit.Assertion
runWithRandomPlane_ dbAction =
    do let biome = Random.weightedSet [(4,TemperateClearing),(1,TemperateForest)]
       plane_ref <- dbNewPlane "testPlane" (TerrainGenerationData 3 biome []) TheUniverse
       dbAction plane_ref

{-- UNIT TESTS BEGIN HERE --}
testcases :: HUnit.Test
testcases = HUnit.TestLabel "root" $ HUnit.TestList [
    HUnit.TestLabel "session"               $ HUnit.TestList $ [testSessionAliveBeforeTimeout, testSessionExpiredAfterTimeout],
    HUnit.TestLabel "database"              $ HUnit.TestList $ [testSetPlayerState, testLocal],
    HUnit.TestLabel "clear-site"            $ testPickRandomClearSite,
    HUnit.TestLabel "Roguestar.Lib.Graph"   $ GraphTests.testcases,
    HUnit.TestLabel "Roguestar.Lib.Core2"   $ Core2Tests.testcases,
    HUnit.TestLabel "Roguestar.Lib.Core"    $ CoreTests.testcases,
    HUnit.TestLabel "Roguestar.Lib.Utility.HierarchicalDatabase" $ HDatabaseTests.testcases]

testSessionAliveBeforeTimeout :: HUnit.Test
testSessionAliveBeforeTimeout = HUnit.TestCase $
    do game_state <- liftIO $ createGameState (GameConfiguration 10 0)
       game_uuid <- liftIO $ createGame (GameConfiguration 10 1) game_state
       m_g <- liftIO $ retrieveGame game_uuid (GameConfiguration 10 9) game_state
       liftIO $ threadDelay 100
       HUnit.assertBool "testSessionAliveBeforeTimeout" ( isJust m_g )

testSessionExpiredAfterTimeout :: HUnit.Test
testSessionExpiredAfterTimeout = HUnit.TestCase $
    do game_state <- liftIO $ createGameState (GameConfiguration 10 0)
       game_uuid <- liftIO $ createGame (GameConfiguration 10 1) game_state
       _ <- liftIO $ createGame (GameConfiguration 10 12) game_state
       liftIO $ threadDelay 100
       m_g2 <- liftIO $ retrieveGame game_uuid (GameConfiguration 10 12) game_state
       HUnit.assertBool "testSessionExpiredAfterTimeout" ( isNothing m_g2 )

-- |
-- Test that we can store and retrieve some simple piece of information in the database.
--
testSetPlayerState :: HUnit.Test
testSetPlayerState = HUnit.TestCase $
    do m_pstate <- liftIO $ flip runDB initial_db $
           do setPlayerState (GameOver PlayerIsVictorious)
              playerState
       case m_pstate of
           Left _ -> HUnit.assertFailure "testSetPlayerState (failed in monad)"
           Right (pstate,_) -> HUnit.assertEqual "testSetPlayState" pstate (GameOver PlayerIsVictorious)

-- |
-- Test that we can execute read-only branches. Changes should not linger after the read-only branch exits.
testLocal :: HUnit.Test
testLocal = HUnit.TestCase $
    do m_pstate <- liftIO $ flip runDB initial_db $
           do local id $ setPlayerState (GameOver PlayerIsVictorious)
              playerState
       case m_pstate of
           Left _ -> HUnit.assertFailure "testLocal (failed in monad)"
           Right (pstate,_) -> HUnit.assertEqual "testLocal" pstate (SpeciesSelectionState Nothing)

testPickRandomClearSite :: HUnit.Test
testPickRandomClearSite = runWithRandomPlanes 10 "testPickRandomClearSite" $ \plane_ref ->
    do Position (x,y) <- pickRandomSite (-1000,1000) (-1000,1000) 50 (areaClearForObjectPlacement 1) plane_ref
       t1 <- terrainAt plane_ref $ Position (x-1,y-1)
       t2 <- terrainAt plane_ref $ Position (x,y-1)
       t3 <- terrainAt plane_ref $ Position (x+1,y-1)
       t4 <- terrainAt plane_ref $ Position (x-1,y)
       t5 <- terrainAt plane_ref $ Position (x,y)
       t6 <- terrainAt plane_ref $ Position (x+1,y)
       t7 <- terrainAt plane_ref $ Position (x-1,y+1)
       t8 <- terrainAt plane_ref $ Position (x,y+1)
       t9 <- terrainAt plane_ref $ Position (x+1,y+1)
       return $ HUnit.assertBool "Unacceptable terrain obstruction." (Prelude.all (not . (`elem` difficult_terrains)) [t1,t2,t3,t4,t5,t6,t7,t8,t9])

