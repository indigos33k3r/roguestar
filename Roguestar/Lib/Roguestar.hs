{-# LANGUAGE Rank2Types, OverloadedStrings #-}

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
     perceive,
     behave,
     Roguestar.Lib.Roguestar.facingBehavior,
     Roguestar.Lib.Roguestar.hasSnapshot,
     popSnapshot,
     getMessages,
     Behavior(..))
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
import Roguestar.Lib.Behavior as Behavior
import Roguestar.Lib.Turns
import Data.Text as T

data Game = Game {
    game_db :: TVar DB_BaseType,
    game_message_text :: TVar [T.Text] }

newGame :: IO Game
newGame =
    do db <- newTVarIO initial_db
       empty_messages <- newTVarIO []
       return $ Game db empty_messages

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
rerollStartingSpecies g =
    do atomically $
           do writeTVar (game_db g) initial_db
              writeTVar (game_message_text g) []
       poke g $
           do species <- pickM all_species
              generateInitialPlayerCreature species
              return species

beginGame :: Game -> IO (Either DBError ())
beginGame g = poke g $ BeginGame.beginGame

perceive :: Game -> (forall m. DBReadable m => DBPerception m a) -> IO (Either DBError a)
perceive g f = peek g $
    do player_creature <- getPlayerCreature
       runPerception player_creature f

-- TODO: this should be moved into the Perception monad
facingBehavior :: Game -> Facing -> IO (Either DBError Behavior)
facingBehavior g facing = peek g $
    do player_creature <- getPlayerCreature
       Behavior.facingBehavior player_creature facing

behave :: Game -> Behavior -> IO (Either DBError ())
behave g b = poke g $
    do player_creature <- getPlayerCreature
       dbPerformPlayerTurn b player_creature

hasSnapshot :: Game -> IO (Either DBError Bool)
hasSnapshot g = peek g DB.hasSnapshot

perceiveSnapshot :: Game -> (forall m. DBReadable m => DBPerception m a) -> IO (Either DBError a)
perceiveSnapshot g f = peek g $ peepOldestSnapshot $
    do player_creature <- getPlayerCreature
       runPerception player_creature f

getSnapshotPlayerState :: Game -> IO (Either DBError PlayerState)
getSnapshotPlayerState g = peek g $ DB.peepOldestSnapshot $ playerState

popSnapshot :: Game -> IO (Either DBError ())
popSnapshot g =
    do msgs <- poke g $
           do msgs <- DB.peepOldestSnapshot unpackMessages
              DB.popOldestSnapshot
              return msgs
       case msgs of
           Right ts -> liftM Right $ mapM_ (putMessage g) ts
           Left e -> return $ Left e

max_messages :: Int
max_messages = 20

putMessage :: Game -> T.Text -> IO ()
putMessage g t = (putStrLn $ T.unpack t) >> (atomically $
    do ts <- readTVar $ game_message_text g
       writeTVar (game_message_text g) $ Prelude.take max_messages $ t:ts)
       
getMessages :: Game -> IO [T.Text]
getMessages g = readTVarIO (game_message_text g)

unpackMessages :: (DBReadable db) => db [T.Text]
unpackMessages =
    do player_state <- playerState
       case player_state of
           SpeciesSelectionState {} -> return []
           PlayerCreatureTurn {} -> return []
           SnapshotEvent evt ->
               do player_creature <- getPlayerCreature
                  runPerception player_creature $ unpackMessages_ evt
           GameOver -> return ["You have been destroyed."]

unpackMessages_ :: (DBReadable m) => SnapshotEvent -> DBPerception m [T.Text]
unpackMessages_ AttackEvent { attack_event_source_creature = c } =
    do player_creature <- whoAmI
       return $ case () of
           () | c == player_creature -> ["The recreant zaps you!"]
           () | otherwise -> ["You zap the recreant!"]
unpackMessages_ MissEvent { miss_event_creature = c } =
    do player_creature <- whoAmI
       return $ case () of
           () | c == player_creature -> ["You try to zap the recreant, but miss."]
           () | otherwise -> ["A recreant tries to zap you, but misses."]
unpackMessages_ KilledEvent { killed_event_creature = c } =
    do player_creature <- whoAmI
       return $ case () of
           () | c == player_creature -> ["You have been destroyed!"]
           () | otherwise -> ["You destroy the recreant!"]
unpackMessages_ WeaponOverheatsEvent {} = return ["Your weapon overheats."]
unpackMessages_ WeaponExplodesEvent {} = return ["Your weapon explodes!"]
unpackMessages_ DisarmEvent {} = return ["Someone disarms someone else."]
unpackMessages_ SunderEvent {} = return ["The weapon has been sundered!"]
unpackMessages_ TeleportEvent {} = return ["You teleport."]
unpackMessages_ ClimbEvent {} = return ["You wonder through a network of tunnels."]
unpackMessages_ HealEvent {} = return ["You heal."]
unpackMessages_ ExpendToolEvent {} = return ["That material sphere has been used up."]
unpackMessages_ BumpEvent {} = return ["You feel more powerful!"]
