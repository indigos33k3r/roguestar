{-# LANGUAGE Rank2Types, OverloadedStrings #-}

module Roguestar.Lib.Roguestar
    (GameConfiguration(..),
     getConfiguration,
     Game,
     GameState,
     createGameState,
     createGame,
     retrieveGame,
     getNumberOfGames,
     getPlayerState,
     getSnapshotPlayerState,
     rerollStartingSpecies,
     Creature(..),
     Terrain(..),
     Position(..),
     Facing(..),
     Roguestar.Lib.Roguestar.beginGame,
     perceive,
     perceiveSnapshot,
     behave,
     Roguestar.Lib.Roguestar.facingBehavior,
     Roguestar.Lib.Roguestar.hasSnapshot,
     popSnapshot,
     getMessages,
     putMessage,
     unpackError,
     Behavior(..))
    where

import System.UUID.V4 as V4
import Data.Map as Map
import Data.List as List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Roguestar.Lib.DB as DB
import Control.Concurrent.STM
import Control.Monad
import Roguestar.Lib.Data.PlayerState
import Roguestar.Lib.SpeciesData
import Roguestar.Lib.Creature
import Roguestar.Lib.CreatureData
import Roguestar.Lib.BeginGame as BeginGame
import Roguestar.Lib.Perception
import Roguestar.Lib.TerrainData
import Roguestar.Lib.Facing
import Roguestar.Lib.Behavior as Behavior
import Roguestar.Lib.Turns
import Roguestar.Lib.Core.Plane
import Data.Text as T
import System.Time
import Control.Concurrent
import Roguestar.Lib.FactionData
import Roguestar.Lib.PlaneVisibility

-- Session timeout information.
data GameConfiguration = GameConfiguration {
    game_config_timeout_seconds :: Integer,
    game_config_current_clock_time_seconds :: Integer }

-- Constructs a GameConfiguration using the current time and the desired session timeout in seconds.
getConfiguration :: Integer -> IO GameConfiguration
getConfiguration timeout_seconds =
    do TOD now _ <- getClockTime
       return $ GameConfiguration timeout_seconds now

-- A collection of games, i.e. all games on the server
data GameState = GameState {
    game_state_gamelist :: TVar (Map.Map BS.ByteString Game),
    game_state_last_cleanup :: TVar Integer }

-- The state information for a specific game.
data Game = Game {
    game_db :: TVar DB_BaseType,
    game_message_text :: TVar [T.Text],
    game_last_touched :: TVar Integer }

newGame :: GameConfiguration -> IO Game
newGame config =
    do db <- newTVarIO initial_db
       empty_messages <- newTVarIO []
       starting_time <- newTVarIO (game_config_current_clock_time_seconds config)
       return $ Game db empty_messages starting_time

createGameState :: GameConfiguration -> IO GameState
createGameState config =
    do gs <- newTVarIO Map.empty
       starting_time <- newTVarIO (game_config_current_clock_time_seconds config)
       return $ GameState gs starting_time

cleanupGameState :: GameConfiguration -> GameState -> IO ()
cleanupGameState config game_state =
    do needs_cleanup <- atomically $
           do last_cleanup_time <- readTVar (game_state_last_cleanup game_state)
              let needs_cleanup = (game_config_current_clock_time_seconds config) > last_cleanup_time + game_config_timeout_seconds config
              when needs_cleanup $ writeTVar (game_state_last_cleanup game_state) (game_config_current_clock_time_seconds config)
              return needs_cleanup
       when needs_cleanup $ 
           do _ <- forkIO $ doCleanup config game_state
              return ()

doCleanup :: GameConfiguration -> GameState -> IO ()
doCleanup config game_state =
    do atomically $
           do game_list <- readTVar $ game_state_gamelist game_state
              forM_ (Map.toList game_list) $ \(key,value) ->
                  do last_touched <- readTVar $ game_last_touched value
                     when (game_config_current_clock_time_seconds config > last_touched + game_config_timeout_seconds config) $
                         writeTVar (game_state_gamelist game_state) =<< liftM (Map.delete key) (readTVar $ game_state_gamelist game_state)

createGame :: GameConfiguration -> GameState -> IO BS.ByteString
createGame config game_state =
    do cleanupGameState config game_state
       new_uuid <- liftM (BS8.pack . show) V4.uuid
       g <- newGame config
       atomically $
           do gs <- readTVar (game_state_gamelist game_state)
              writeTVar (game_state_gamelist game_state) $ Map.insert new_uuid g gs
       return new_uuid

retrieveGame :: BS.ByteString -> GameConfiguration -> GameState -> IO (Maybe Game)
retrieveGame existing_uuid config game_state =
    do cleanupGameState config game_state
       atomically $
           do m_g <- liftM (Map.lookup existing_uuid) $ readTVar (game_state_gamelist game_state)
              case m_g of
                  Just g -> writeTVar (game_last_touched g) (game_config_current_clock_time_seconds config)
                  Nothing -> return ()
              return m_g

getNumberOfGames :: GameState -> IO Integer
getNumberOfGames game_state = atomically $ liftM (toInteger . Map.size) $ readTVar (game_state_gamelist game_state)

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
       result <- flip runDB game $
           do result <- f
              cleanupNonPlayerSnapshots
              return result
       case result of
           Left err -> return $ Left err
           Right (a,next_db) ->
               do atomically $ writeTVar (game_db g) next_db
                  return $ Right a

cleanupNonPlayerSnapshots :: DB ()
cleanupNonPlayerSnapshots =
    do has_snapshot <- DB.hasSnapshot
       is_relevant <- DB.peepOldestSnapshot $
           do participants <- liftM (List.map genericReference . participantsOf) playerState
              m_plane_ref <- getCurrentPlane
              case m_plane_ref of
                  _ | List.null participants -> return False
                  Nothing                    -> return True
                  Just plane_ref ->
                      liftM (not . List.null) $ dbGetVisibleObjectsForFaction
                          (return . (`elem` participants))
                          Player
                          plane_ref
       when (has_snapshot && not is_relevant) $
           do DB.popOldestSnapshot
              cleanupNonPlayerSnapshots

getPlayerState :: Game -> IO (Either DBError PlayerState)
getPlayerState g = peek g playerState

rerollStartingSpecies :: Game -> IO (Either DBError Species)
rerollStartingSpecies g =
    do atomically $
           do writeTVar (game_db g) initial_db
              writeTVar (game_message_text g) []
       poke g $
           do species <- weightedPickM $ unweightedSet all_species
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
putMessage g t = atomically $
    do ts <- readTVar $ game_message_text g
       writeTVar (game_message_text g) $ Prelude.take max_messages $ t:ts

getMessages :: Game -> IO [T.Text]
getMessages g = readTVarIO (game_message_text g)

unpackError :: ErrorFlag -> T.Text
unpackError BuildingApproachWrongAngle = "Nothing happens."
unpackError x = T.concat ["An unknown error occured: ", T.pack $ show x]

unpackMessages :: (DBReadable db) => db [T.Text]
unpackMessages =
    do player_state <- playerState
       case player_state of
           SpeciesSelectionState {} -> return []
           PlayerCreatureTurn {} -> return []
           SnapshotEvent evt ->
               do player_creature <- getPlayerCreature
                  runPerception player_creature $ unpackMessages_ evt
           GameOver PlayerIsDead -> return ["You have been destroyed."]
           GameOver PlayerIsVictorious -> return ["You have transcended your programming!"]

unpackMessages_ :: (DBReadable m) => SnapshotEvent -> DBPerception m [T.Text]
unpackMessages_ AttackEvent { attack_event_target_creature = c } =
    do player_creature <- whoAmI
       return $ case () of
           () | c == player_creature -> ["The recreant zaps you!"]
           () | otherwise -> ["You zap the recreant!"]
unpackMessages_ MissEvent { miss_event_creature = c } =
    do player_creature <- whoAmI
       return $ case () of
           () | c == player_creature -> ["You miss."]
           () | otherwise -> ["The recreant misses."]
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
unpackMessages_ SpawnEvent {} = return ["A recreant materializes on the teleportation pad!"]
unpackMessages_ ClimbEvent {} = return ["You wonder through a network of tunnels."]
unpackMessages_ HealEvent {} = return ["You heal."]
unpackMessages_ ExpendToolEvent {} = return ["That material sphere has been used up."]
unpackMessages_ BumpEvent {} = return ["You feel more powerful!"]
