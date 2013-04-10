module Roguestar.Lib.Data.CharacterData
    (applyCharacterClass)
    where

import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.PersistantData

applyCharacterClass :: CharacterClass -> Creature -> Creature
applyCharacterClass character_class creature = applyToCreature (CharacterClass character_class : classInfo character_class) creature

classInfo :: CharacterClass -> [CreatureTrait]

-------------------------------------------------------------------------------
--
--  Special Classes
--
--  These are special character classes that are gained by taking specific actions.
--
-------------------------------------------------------------------------------

classInfo StarChild = [Aggression,Perception]

