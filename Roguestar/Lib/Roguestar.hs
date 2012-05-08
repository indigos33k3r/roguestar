module Roguestar.Lib.Roguestar
    (Game,
     newEmptyGame)
    where

import Roguestar.Lib.DB as DB
import Control.Concurrent.STM

data Game = Game {
    game_db :: TVar DB_BaseType }

newEmptyGame :: IO Game
newEmptyGame =
    do db <- newTVarIO initial_db
       return $ Game db

