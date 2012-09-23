{-# LANGUAGE OverloadedStrings #-}

module Roguestar.Lib.UnitTests (runTests) where

import Data.Text as T
import Control.Monad.Writer.Lazy as W
import Roguestar.Lib.Roguestar
import Data.Maybe
import Control.Concurrent
import Data.Monoid
import System.IO

type UnitTest = WriterT (T.Text,All) IO ()

runTests :: IO (T.Text,Bool)
runTests =
    do ((),(t,All b)) <- runWriterT $ sequence_ unit_tests
       return (t,b)

unit_tests :: [UnitTest]
unit_tests = [testSessionAliveBeforeTimeout,
              testSessionExpiredAfterTimeout]

assert :: Bool -> T.Text -> UnitTest
assert ok test_name =
    do let message = test_name `T.append` (if ok then ": ok." else ": FAILED.") `T.append` "\n"
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
