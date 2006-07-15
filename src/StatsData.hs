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

module StatsData
    (Stats(..),
     stats,
     modStr,
     modDex,
     modCon,
     modInt,
     modPer,
     modCha,
     modMind)
    where

-- |
-- Represents the seven roguestar creature statistics:
-- Strength (str)
-- Dexterity (dex)
-- Constitution (con)
-- Intelligence (int)
-- Perception (per)
-- Charisma (cha)
-- Mindfulness (min)
--

data Stats = Stats {str, dex, con, int, per, cha, mind :: Integer} deriving (Show, Read)

-- |
-- Used to generate a Stats object with all the same stats (i.e. stats 1 => Stats 1 1 1 1 1 1 1)
--

stats :: Integer -> Stats
stats x = (Stats {str=x, dex=x, con=x, int=x, per=x, cha=x, mind=x})

-- |
-- Functions to modify a single stat in a Stats block.
--
modStr :: Integer -> Stats -> Stats
modStr x st = st { str = x }

modDex :: Integer -> Stats -> Stats
modDex x st = st { dex = x }

modCon :: Integer -> Stats -> Stats
modCon x st = st { con = x }

modInt :: Integer -> Stats -> Stats
modInt x st = st { int = x }

modPer :: Integer -> Stats -> Stats
modPer x st = st { per = x }

modCha :: Integer -> Stats -> Stats
modCha x st = st { cha = x }

modMind :: Integer -> Stats -> Stats
modMind x st = st { mind = x }
