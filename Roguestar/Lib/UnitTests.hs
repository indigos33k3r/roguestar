{-# LANGUAGE OverloadedStrings #-}

module Roguestar.Lib.UnitTests (runTests) where

import Data.Text as T
import Control.Monad.Writer.Lazy as W
import Roguestar.Lib.Roguestar
import Data.Maybe
import Control.Concurrent
import Data.Monoid
import System.IO
import Roguestar.Lib.DB
import Roguestar.Lib.PlayerState
import Control.Monad.Reader.Class

type UnitTest = WriterT (T.Text,All) IO ()

runTests :: IO (T.Text,Bool)
runTests =
    do ((),(t,All b)) <- runWriterT $ sequence_ unit_tests
       return (t,b)

unit_tests :: [UnitTest]
unit_tests = [testSessionAliveBeforeTimeout,
              testSessionExpiredAfterTimeout,
              testSetPlayerState,
              testLocal]

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
           Left err -> assert False "testSetPlayerState (failed in monad)"
           Right (pstate,_) -> assertEqual pstate (GameOver PlayerIsVictorious) "testSetPlayerState"

testLocal :: UnitTest
testLocal =
    do m_pstate <- liftIO $ flip runDB initial_db $
           do local id $ setPlayerState (GameOver PlayerIsVictorious)
              playerState
       case m_pstate of
           Left err -> assert False "testLocal (failed in monad)"
           Right (pstate,_) -> assertEqual pstate (SpeciesSelectionState Nothing) "testLocal"


