
module Roguestar.Lib.Character
    (applyCharacterClass)
    where

import Roguestar.Lib.Alignment
import Roguestar.Lib.CreatureData
import Roguestar.Lib.TerrainData
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

