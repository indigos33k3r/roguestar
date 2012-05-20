{-# LANGUAGE Rank2Types #-}

module Roguestar.Lib.Roguestar
    (Game,
     newGame,
     getPlayerState,
     rerollStartingSpecies,
     Creature(..),
     TerrainPatch(..),
     Position(..),
     Facing(..),
     Roguestar.Lib.Roguestar.beginGame,
     perceive)
    where

import Roguestar.Lib.DB as DB
import Control.Concurrent.STM
import Control.Monad
import Roguestar.Lib.PlayerState
import Roguestar.Lib.SpeciesData
import Roguestar.Lib.Random
import Roguestar.Lib.Creature
import Roguestar.Lib.CreatureData
import Roguestar.Lib.BeginGame as BeginGame
import Roguestar.Lib.Perception
import Roguestar.Lib.TerrainData
import Roguestar.Lib.Facing

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

rerollStartingSpecies :: Game -> IO (Either DBError Species)
rerollStartingSpecies g = poke g $
    do species <- pickM all_species
       generateInitialPlayerCreature species
       return species

beginGame :: Game -> IO (Either DBError ())
beginGame g = poke g $ BeginGame.beginGame

perceive :: Game -> (forall m. DBReadable m => DBPerception m a) -> IO (Either DBError a)
perceive g f = peek g $
    do player_creature <- maybe (fail "No player creature selected yet.") return =<< getPlayerCreature
       runPerception player_creature f

