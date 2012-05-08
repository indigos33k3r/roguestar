
module Roguestar.Lib.CharacterData
    (CharacterClass(..),
     all_character_classes,
     base_character_classes)
    where

import Roguestar.Lib.PersistantData

all_character_classes :: [CharacterClass]
all_character_classes = [minBound..maxBound]

base_character_classes :: [CharacterClass]
base_character_classes = [Barbarian,
                          Consular,
                          Engineer,
                          ForceAdept,
                          Marine,
                          Ninja,
                          Pirate,
                          Scout,
                          Shepherd,
                          Thief,
                          Warrior]

