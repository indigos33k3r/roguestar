module Roguestar.Lib.Data.CharacterData
    (applyCharacterClass)
    where

import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.Data.PersistantData

applyCharacterClass :: CharacterClass -> MonsterData -> MonsterData
applyCharacterClass character_class creature = applyToMonster (CharacterClass character_class : classInfo character_class) creature

classInfo :: CharacterClass -> [MonsterTrait]

-------------------------------------------------------------------------------
--
--  Special Classes
--
--  These are special character classes that are gained by taking specific actions.
--
-------------------------------------------------------------------------------

classInfo StarChild = [Aggression,Perception]

