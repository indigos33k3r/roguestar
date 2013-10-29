{-# LANGUAGE OverloadedStrings #-}

module Roguestar.Lib.UnitTests (runTests) where

import Data.Text as T
import Control.Monad.Writer.Lazy as W
import Roguestar.Lib.Roguestar
import Data.Maybe
import Control.Concurrent
import System.IO
import Roguestar.Lib.DB
import Roguestar.Lib.Data.PlayerState
import Control.Monad.Reader.Class
import Roguestar.Lib.Core.Plane
import Roguestar.Lib.Data.TerrainData
import Roguestar.Lib.Utility.SiteCriteria
import Roguestar.Lib.Random as Random

import qualified Test.HUnit.Base as HUnit
import qualified Test.HUnit.Text as HUnitText
import qualified Roguestar.Lib.Model.Tests as ModelTests
import qualified Roguestar.Lib.Core.Tests as CoreTests

type UnitTest = WriterT (T.Text,All) IO ()

runTests :: IO (T.Text,Bool)
runTests =
    do ((),(t,All b)) <- runWriterT $ sequence_ unit_tests
       counts <- HUnitText.runTestTT testcases
       return (t,b && HUnit.errors counts > 0 || HUnit.failures counts > 0)

unit_tests :: [UnitTest]
unit_tests = [testPickRandomClearSite]

assert :: Bool -> T.Text -> UnitTest
assert ok test_name =
    do let message = test_name `T.append` (if ok then ": ok." else ": FAILED.") `T.append` "\n"
       tell (message, All ok)
       liftIO $ hPutStr stderr $ T.unpack message

-- Generate N random planes and run tests against them.
runWithRandomPlanes :: Int -> T.Text -> (PlaneRef -> DB Bool) -> UnitTest
runWithRandomPlanes n test_name db_action = forM_ [1..n] $ \x ->
    do b <- liftIO $ runDB (runWithRandomPlane_ db_action) initial_db
       assert (either (const False) fst b) (test_name `T.append` "#" `T.append` T.pack (show x))

runWithRandomPlane_ :: (PlaneRef -> DB Bool) -> DB Bool
runWithRandomPlane_ dbAction =
    do let biome = Random.weightedSet [(4,TemperateClearing),(1,TemperateForest)]
       plane_ref <- dbNewPlane "testPlane" (TerrainGenerationData 3 biome []) TheUniverse
       dbAction plane_ref

{-- UNIT TESTS BEGIN HERE --}
testcases :: HUnit.Test
testcases = HUnit.TestList [
    HUnit.TestLabel "session"               $ HUnit.TestList $ [testSessionAliveBeforeTimeout, testSessionExpiredAfterTimeout],
    HUnit.TestLabel "database"              $ HUnit.TestList $ [testSetPlayerState, testLocal],
    HUnit.TestLabel "Roguestar.Lib.Model"   $ ModelTests.testcases,
    HUnit.TestLabel "Roguestar.Lib.Core"    $ CoreTests.testcases]

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

testPickRandomClearSite :: UnitTest
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
       return $ Prelude.all (not . (`elem` difficult_terrains)) [t1,t2,t3,t4,t5,t6,t7,t8,t9]
