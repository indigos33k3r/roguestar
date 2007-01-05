--------------------------------------------------------------------------
--  roguestar-gl: the space-adventure roleplaying game OpenGL frontend.   
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

module Globals
    (RoguestarGlobals(..),
     roguestar_globals_0,
     RoguestarEngineState(..))
    where

import Data.IORef
import Data.Map as Map
import Graphics.UI.GLUT
import Translation
import PrintTextData
import Quality
import Tables
import DefaultKeymap
import Math3D
import CameraTracking
import Models.LibraryData

data RoguestarEngineState = RoguestarEngineState { restate_tables :: [RoguestarTable], restate_answers :: [(String,String)] }

-- |
-- Some nasty global variables that we can't seem to live without.
--
-- [@global_quality@] the graphics quality setting (this should never change; it's set by a command line option)
-- [@global_display_func@] the OpenGL display function (change this using the accessor function FIXME no accessor function yet)
-- [@global_text_output_buffer@] text that has been printed to the screen (don't touch unless you're PrintText.hs)
-- [@global_text_output_mode@] whether we're showing an entire screen of text or just a few lines (or nothing) (don't touch unless you're PrintText.hs)
-- [@global_engine_input_lines@] text input from the engine (don't touch unless you're Driver.hs)
-- [@global_engine_input_line_fragment@] the current string being read (don't touch unless you're Driver.hs)
-- [@global_engine_output_lines@] lines that have already been sent to stdout (don't touch unless you're Driver.hs), used to ensure that we don't send the same request many times
-- [@global_engine_state@] raw state information pulled from the engine (use accessor functions in Driver.hs)
-- [@global_old_engine_state@] one-turn-old engine state (use accessor functions in Driver.hs)
-- [@global_stale@] have we sent an action but not yet recieved a done; if so our engine state variables are out of date but havn't been replaced by anything yet (don't touch unless you're Driver.hs)
-- [@global_language@] language (anyone can read, only main function should write based on command line arguments)
-- [@global_keymap@] mapping from keystrokes to action names, only (don't touch unless you're Main.hs)
-- [@global_user_input@] input from the user typing (don't touch unless you're Main.hs)
-- [@global_dones@] number of \"done\" lines recieved from the engine, used to track turn changes.
-- [@global_terrain_rendering_function@] function to draw terrain
data RoguestarGlobals = RoguestarGlobals {
					  global_quality :: Quality,
					  global_display_func :: IORef RoguestarGlobals -> IO (),
					  global_text_output_buffer :: [(TextType,String)], -- in reverse order for ease of appending
					  global_text_output_mode :: PrintTextMode,
					  global_engine_input_lines :: [String], -- in reverse order for ease of appending (but each string is in forward order)
					  global_engine_input_line_fragment :: String, -- in reverse order for easy of appending
					  global_engine_output_lines :: [String],
					  global_engine_state :: RoguestarEngineState,
					  global_old_engine_state :: RoguestarEngineState,
					  global_stale :: Bool,
					  global_language :: Language,
					  global_keymap :: [(String,String)], -- map of keystrokes to action names
					  global_user_input :: String, -- in normal order
					  global_dones :: Integer,
					  global_terrain_rendering_function :: IORef RoguestarGlobals -> Bool -> IO (),
					  global_terrain_data :: Map (Integer,Integer) String,
					  global_last_camera_update_seconds :: Rational,
					  global_camera :: Camera,
					  global_library_models :: Map (LibraryModel,Quality) DisplayList -- remember models that have been stored in a display list
					 }

-- |
-- Default starting values for all globals.
--
roguestar_globals_0 :: RoguestarGlobals
roguestar_globals_0 = RoguestarGlobals {
					global_quality = Good,
					global_display_func = \_ -> return (),
					global_text_output_buffer = [],
					global_text_output_mode = Unlimited,
					global_engine_input_lines = [],
					global_engine_input_line_fragment = "",
					global_engine_output_lines = [],
					global_engine_state = RoguestarEngineState { restate_tables = [], restate_answers = [] },
					global_old_engine_state = RoguestarEngineState { restate_tables = [], restate_answers = [] },
					global_stale = False,
					global_language = English,
					global_keymap = default_keymap,
					global_user_input = [],
					global_dones = 0,
					global_terrain_rendering_function = \_ _ -> return (),
					global_terrain_data = Map.fromList [],
					global_last_camera_update_seconds = 0,
					global_camera = Camera (Point3D 0 0 0) (Point3D 0 3 (-3)),
					global_library_models = empty
				       }
