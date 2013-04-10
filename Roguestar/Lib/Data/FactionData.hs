{-# LANGUAGE OverloadedStrings #-}
--Data
module Roguestar.Lib.Data.FactionData
    (Faction(..),factionPrefix)
    where

import qualified Data.ByteString.Char8 as B

data Faction = Player
             | Nonaligned  -- friendly, aggroed per-planet
             | Monsters    -- always hostile
             | Cyborgs     -- cyborgs
               deriving (Eq,Read,Show,Enum,Bounded)

factionPrefix :: Faction -> B.ByteString
factionPrefix Player = "Z"
factionPrefix Monsters = "M"
factionPrefix Nonaligned = "P"
factionPrefix Cyborgs = "Y"
