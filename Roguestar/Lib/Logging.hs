--Services
module Roguestar.Lib.Logging
    (initLogging,
     gameplay_log,
     module System.Log.Logger)
    where

import System.Log.Logger
import System.Log.Handler.Simple

initLogging :: Priority -> IO ()
initLogging prio =
    do logger <- fileHandler "log/roguestar.log" prio
       updateGlobalLogger rootLoggerName $ setHandlers [logger]
       updateGlobalLogger rootLoggerName (setLevel prio)
       logM gameplay_log EMERGENCY "Initializing log."
       logM gameplay_log WARNING "Logging warnings."
       logM gameplay_log INFO "Logging informational messages."
       logM gameplay_log DEBUG "Logging debug messages."

gameplay_log :: String
gameplay_log = "roguestar.gameplay"

