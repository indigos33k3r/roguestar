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
import Roguestar.Lib.TerrainData
import Roguestar.Lib.Utility.SiteCriteria
import Roguestar.Lib.Random as Random

type UnitTest = WriterT (T.Text,All) IO ()

runTests :: IO (T.Text,Bool)
runTests =
    do ((),(t,All b)) <- runWriterT $ sequence_ unit_tests
       return (t,b)

unit_tests :: [UnitTest]
unit_tests = [testSessionAliveBeforeTimeout,
              testSessionExpiredAfterTimeout,
              testSetPlayerState,
              testLocal,
              testPickRandomClearSite]

assert :: Bool -> T.Text -> UnitTest
assert ok test_name =
    do let message = test_name `T.append` (if ok then ": ok." else ": FAILED.") `T.append` "\n"
       tell (message, All ok)
       liftIO $ hPutStr stderr $ T.unpack message

assertEqual :: (Show a,Eq a) => a -> a -> T.Text -> UnitTest
assertEqual actual expected test_name =
    do let ok = actual == expected
           message = test_name `T.append` (if ok then ": ok." else ": FAILED." `T.append` "\n"
                                   `T.append`
                     ("Actual:   " `T.append` T.pack (show actual) `T.append` "\n")
                                   `T.append`
                     ("Expected: " `T.append` T.pack (show expected))) `T.append` "\n"
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
testSessionAliveBeforeTimeout :: UnitTest
testSessionAliveBeforeTimeout =
    do game_state <- liftIO $ createGameState (GameConfiguration 10 0)
       game_uuid <- liftIO $ createGame (GameConfiguration 10 1) game_state
       m_g <- liftIO $ retrieveGame game_uuid (GameConfiguration 10 9) game_state
       liftIO $ threadDelay 100
       assert ( isJust m_g ) "testSessionAliveBeforeTimeout"

testSessionExpiredAfterTimeout :: UnitTest
testSessionExpiredAfterTimeout =
    do game_state <- liftIO $ createGameState (GameConfiguration 10 0)
       game_uuid <- liftIO $ createGame (GameConfiguration 10 1) game_state
       _ <- liftIO $ createGame (GameConfiguration 10 12) game_state
       liftIO $ threadDelay 100
       m_g2 <- liftIO $ retrieveGame game_uuid (GameConfiguration 10 12) game_state
       assert ( isNothing m_g2 ) "testSessionExpiredAfterTimeout"

testSetPlayerState :: UnitTest
testSetPlayerState =
    do m_pstate <- liftIO $ flip runDB initial_db $
           do setPlayerState (GameOver PlayerIsVictorious)
              playerState
       case m_pstate of
           Left _ -> assert False "testSetPlayerState (failed in monad)"
           Right (pstate,_) -> assertEqual pstate (GameOver PlayerIsVictorious) "testSetPlayerState"

testLocal :: UnitTest
testLocal =
    do m_pstate <- liftIO $ flip runDB initial_db $
           do local id $ setPlayerState (GameOver PlayerIsVictorious)
              playerState
       case m_pstate of
           Left _ -> assert False "testLocal (failed in monad)"
           Right (pstate,_) -> assertEqual pstate (SpeciesSelectionState Nothing) "testLocal"

testPickRandomClearSite :: UnitTest
testPickRandomClearSite = runWithRandomPlanes 10 "testPickRandomClearSite" $ \plane_ref ->
    do Position (x,y) <- pickRandomSite (-1000,100) (-1000,100) 50 (areaClearForObjectPlacement 1) plane_ref
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
