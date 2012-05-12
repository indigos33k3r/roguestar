module Roguestar.Lib.Roguestar
    (Game,
     newGame,
     getPlayerState,
     Roguestar.Lib.Roguestar.getStartingSpecies)
    where

import Roguestar.Lib.DB as DB
import Control.Concurrent.STM
import Roguestar.Lib.PlayerState
import Roguestar.Lib.SpeciesData
import Roguestar.Lib.Random
import Roguestar.Lib.Creature

data Game = Game {
    game_db :: TVar DB_BaseType }

newGame :: IO Game
newGame =
    do db <- newTVarIO initial_db
       return $ Game db

peek :: Game -> DB a -> IO (Either DBError a)
peek g f =
    do game <- atomically $ readTVar (game_db g)
       result <- runDB f game
       return $ case result of
           Left err -> Left err
           Right (a,_) -> Right a

poke :: Game -> DB a -> IO (Either DBError a)
poke g f =
    do game <- atomically $ readTVar (game_db g)
       result <- runDB f game
       case result of
           Left err -> return $ Left err
           Right (a,next_db) ->
               do atomically $ writeTVar (game_db g) next_db
                  return $ Right a

getPlayerState :: Game -> IO (Either DBError PlayerState)
getPlayerState g = peek g playerState

getStartingSpecies :: Game -> IO (Either DBError (Maybe Species))
getStartingSpecies g = peek g DB.getStartingSpecies

rerollStartingSpecies :: Game -> Species -> IO (Either DBError Species)
rerollStartingSpecies g species = poke g $
    do species <- pickM all_species
       generateInitialPlayerCreature species
       return species

