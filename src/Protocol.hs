--------------------------------------------------------------------------
--  roguestar-engine: the space-adventure roleplaying game backend.       
--  Copyright (C) 2006 Christopher Lane Hinson <lane@downstairspeople.org>  
--                                                                        
--  This program is free software; you can redistribute it and/or modify  
--  it under the terms of the GNU General Public License as published by  
--  the Free Software Foundation; either version 2 of the License, or     
--  (at your option) any later version.                                   
--                                                                        
--  This program is distributed in the hope that it will be useful,       
--  but WITHOUT ANY WARRANTY; without even the implied warranty of        
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         
--  GNU General Public License for more details.                          
--                                                                        
--  You should have received a copy of the GNU General Public License along  
--  with this program; if not, write to the Free Software Foundation, Inc.,  
--  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.           
--                                                                        
--------------------------------------------------------------------------

module Protocol
    (mainLoop)
    where

import Data.Char
import Data.List as List
import Control.Monad.State
import CreatureData
import Creature
import Character
import StatsData
import DB
import DBData
import System.Exit
import Races
import System.IO
import BeginGame
import Data.Maybe
import Plane
import FactionData
import PlaneVisibility

mainLoop :: DB_BaseType -> IO ()
mainLoop db0 = do next_command <- getLine
		  db1 <- ioDispatch (words $ map toLower next_command) db0
		  putStrLn "over"
		  hFlush stdout
		  mainLoop db1

done :: DB String
done = return "done"

-- |
-- Runs a database action assuming the database is in the DBRaceSelectionState,
-- otherwise returns an error message.
--
dbRequiresRaceSelectionState :: DB String -> DB String
dbRequiresRaceSelectionState action = do state <- dbState
					 case state of
						    DBRaceSelectionState -> action
						    _ -> return "protocol-error: not in race selection state"

-- |
-- Runs a database action assuming the database is in the DBClassSelectionState,
-- otherwise returns an error message.
--
dbRequiresClassSelectionState :: (Creature -> DB String) -> DB String
dbRequiresClassSelectionState action = do state <- dbState
					  case state of
						     DBClassSelectionState creature -> action creature
						     _ -> return "protocol-error: not in class selection state"

dbRequiresPlayerCenteredState :: (Creature -> DB String) -> DB String
dbRequiresPlayerCenteredState action =
    do state <- dbState
       case state of
		  DBClassSelectionState creature -> action creature
		  DBPlayerCreatureTurn creature_ref -> action =<< dbGetCreature creature_ref
		  _ -> return "protocol-error: not in player-centered state"

dbRequiresPlanarTurnState :: (CreatureRef -> DB String) -> DB String
dbRequiresPlanarTurnState action =
    do state <- dbState
       case state of
		  DBPlayerCreatureTurn creature_ref -> action creature_ref
		  _ -> return "protocol-error: not in planar turn state"

ioDispatch :: [String] -> DB_BaseType -> IO DB_BaseType

ioDispatch ["quit"] _ = exitWith ExitSuccess

ioDispatch ["reset"] _ = do db0 <- initialDB
			    putStrLn "done"
			    return db0

ioDispatch ("game":args) db0 = let (outstr,db1) = runState (dbDispatch args) db0
				    in do putStrLn (map toLower outstr)
					  return db1

ioDispatch ("save":_) db0 = do putStrLn "error: save not implemented"
			       return db0

ioDispatch ("load":_) db0 = do putStrLn "error: load not implemented"
			       return db0

ioDispatch ("noop":_) db0 = return db0

ioDispatch unknown_command db0 = do putStrLn ("protocol-error: unknown command " ++ (unwords unknown_command))
				    return db0

dbDispatch :: [String] -> DB String

dbDispatch ["query","state"] = 
    do state <- dbState
       return $ case state of
			   DBRaceSelectionState -> "answer: state race-selection"
			   DBClassSelectionState {} -> "answer: state class-selection"
			   DBPlayerCreatureTurn {} -> "answer: state player-turn"

dbDispatch ["query","player-races"] =
    return ("begin-table player-races 0 name\n" ++
	    unlines player_race_names ++
	    "end-table")

dbDispatch ["query","visible-terrain"] =
    do maybe_plane_ref <- dbGetCurrentPlane
       terrain_map <- maybe (return []) (dbGetVisibleTerrainForFaction Player) maybe_plane_ref 
       return ("begin-table visible-terrain 0 x y terrain-type\n" ++
	       (unlines $ map (\ ((x,y),terrain_type) -> unwords [show x, show y, show terrain_type]) terrain_map) ++
	       "end-table")

dbDispatch ["action","select-race",race_name] = 
    dbRequiresRaceSelectionState $ dbSelectPlayerRace race_name

dbDispatch ["action","reroll"] =
    dbRequiresClassSelectionState $ dbRerollRace

dbDispatch ["action","select-class",class_name] =
    dbRequiresClassSelectionState $ dbSelectPlayerClass class_name

dbDispatch ["query","player-stats"] = dbRequiresPlayerCenteredState dbQueryPlayerStats

dbDispatch ["query","center-coordinates"] = dbRequiresPlanarTurnState dbQueryCenterCoordinates

dbDispatch ["query","base-classes"] = dbRequiresClassSelectionState dbQueryBaseClasses

dbDispatch unrecognized = return ("protocol-error: unrecognized request `" ++ (unwords unrecognized) ++ "`")

dbSelectPlayerRace :: String -> DB String
dbSelectPlayerRace race_name = case (selectPlayerRace race_name)
			       of
			       Nothing -> return ("protocol-error: unrecognized race '" ++ race_name ++ "'")
			       Just species -> do dbGenerateInitialPlayerCreature species
						  done

dbSelectPlayerClass :: String -> Creature -> DB String
dbSelectPlayerClass class_name creature = 
    let eligable_base_classes = getEligableBaseCharacterClasses creature
	in case findIndex (\x -> (map toLower . show) x == class_name) eligable_base_classes
	   of
	   Nothing -> return ("protocol-error: unrecognized or invalid class '" ++ class_name ++ "'")
	   Just i -> do dbBeginGame creature (eligable_base_classes !! i)
			done

dbRerollRace :: Creature -> DB String
dbRerollRace _ = do starting_race <- dbGetStartingRace
		    dbGenerateInitialPlayerCreature $ fromJust starting_race
		    done

dbQueryPlayerStats :: Creature -> DB String
dbQueryPlayerStats creature = return $ playerStatsTable creature

playerStatsTable :: Creature -> String
playerStatsTable c =
    "begin-table player-stats 0 property value\n" ++
               "str " ++ (show $ str c) ++ "\n" ++
	       "dex " ++ (show $ dex c) ++ "\n" ++
	       "con " ++ (show $ con c) ++ "\n" ++
	       "int " ++ (show $ int c) ++ "\n" ++
	       "per " ++ (show $ per c) ++ "\n" ++
	       "cha " ++ (show $ cha c) ++ "\n" ++
	       "mind " ++ (show $ mind c) ++ "\n" ++
	       "hp " ++ (show $ creatureScore HitPoints c) ++ "\n" ++
	       "maxhp " ++ (show $ creatureScore MaxHitPoints c) ++ "\n" ++
	       "species " ++ (creature_species_name c) ++ "\n" ++
	       "random-id " ++ (show $ creature_random_id c) ++ "\n" ++
	       "effective-level " ++ (show $ creatureScore EffectiveLevel c) ++ "\n" ++
	       "gender " ++ (show $ creatureGender c) ++ "\n" ++
	       "end-table"

dbQueryBaseClasses :: Creature -> DB String
dbQueryBaseClasses creature = return $ baseClassesTable creature

baseClassesTable :: Creature -> String
baseClassesTable creature = 
    "begin-table base-classes 0 class\n" ++
    (unlines $ map show $ getEligableBaseCharacterClasses creature) ++
    "end-table"

dbQueryCenterCoordinates :: CreatureRef -> DB String
dbQueryCenterCoordinates creature_ref =
    do maybe_loc <- dbWhere creature_ref
       case (return . snd) =<< maybe_loc of
		Just (DBCoordinateLocation (x,y)) -> return (begin_table ++
							     "x " ++ show x ++ "\n" ++
							     "y " ++ show y ++ "\n" ++
							     "end-table")
		_ -> return (begin_table ++ "end-table")
	   where begin_table = "begin-table center-coordinates 0 axis coordinate\n"