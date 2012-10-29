--Services
module Roguestar.Lib.Logging
    (initLogging,
     log_creature,
     log_database,
     log_plane,
     log_travel,
     log_turns,
     log_behavior,
     module System.Log.Logger)
    where

import System.Log.Logger
import System.Log.Handler.Simple

initLogging :: Priority -> IO ()
initLogging prio = 
    do logger <- fileHandler "log/roguestar.log" prio
       updateGlobalLogger rootLoggerName $ setHandlers [logger]

log_creature :: String
log_creature = "lib.Creature"

log_database :: String
log_database = "lib.DB"

log_plane :: String
log_plane = "lib.Plane"

log_travel :: String
log_travel = "lib.Travel"

log_turns :: String
log_turns = "lib.Turns"

log_behavior :: String
log_behavior = "lib.Behavior"

