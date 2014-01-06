module Roguestar.Lib.Behavior.CharacterAdvancement
    (CharacterBumpResult(..),
     characterFitness,
     bumpCharacter,
     characterLevel,
     newCharacterLevel,
     newCharacterClass)
    where

import qualified Data.Map as Map
import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.Data.PowerUpData
import Roguestar.Lib.Data.PersistantData

-- TODO: rename "bump" to more clear language, like "advance" or "advanceCharacterLevel".

data CharacterBumpResult =
    CharacterAwarded  { character_points_awarded :: Integer,
                        character_new :: MonsterData }
  | CharacterAdvanced { character_new_level :: Integer,
                        character_new :: MonsterData }
  | CharacterForced   { character_new_character_class :: CharacterClass,
                        character_new :: MonsterData }

-- |
-- Increases the character score by the set amount.
-- If the score is high enough that the character can advance to the next level,
-- this function will apply that advancement.
--
bumpCharacter :: PowerUpData -> MonsterData -> CharacterBumpResult
bumpCharacter (ForceCharacter character_class) c =
        if CharacterClass character_class `elem` Map.keys (creature_traits c)
            then bumpCharacter (AwardCharacter $ characterFitness new_character - characterFitness c) c
            else CharacterForced {
                character_new_character_class = character_class,
                character_new = new_character }
    where new_character = applyToMonster character_class c
bumpCharacter (AwardCharacter n) c =
        if fitness_gain >= bumped_score
            then CharacterAdvanced {
                character_new_level = characterLevel new_character,
                character_new = new_character { creature_points = bumped_score - fitness_gain } }
            else CharacterAwarded {
                character_points_awarded = n,
                character_new = c { creature_points = bumped_score } }
    where bumped_score = creature_points c + n
          fitness_gain = characterFitness new_character - characterFitness c
          new_character = applyToMonster (Map.keys $ creature_traits c) c

newCharacterClass :: CharacterBumpResult -> Maybe CharacterClass
newCharacterClass (CharacterForced character_class _) = Just character_class
newCharacterClass _ = Nothing

newCharacterLevel :: CharacterBumpResult -> Maybe Integer
newCharacterLevel (CharacterAdvanced new_level _) = Just new_level
newCharacterLevel _ = Nothing

-- |
-- Answers the character level.  This is the maximum of the number
-- of levels the Character has in any class.
-- A rather arbitrary (non-representative of game balance)
-- measure of Character power.
--
characterLevel :: MonsterData -> Integer
characterLevel = maximum . Map.elems . creature_traits

-- |
-- Answers the estimated fitness (powerfulness) of the Character.
--
characterFitness :: MonsterData -> Integer
characterFitness c = sum $ (Map.elems $ creature_traits c)

