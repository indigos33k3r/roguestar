
module Roguestar.Lib.CreatureData
    (Creature(..),
     CreatureTrait(..),
     CreatureInteractionMode(..),
     CreatureAbility(..),
     CreatureEndo(..),
     CreatureScore(..),
     CreatureHealth(..),
     creatureHealth,
     creatureAbilityScore,
     empty_creature)
    where

import Roguestar.Lib.PersistantData
import Roguestar.Lib.Alignment
import Data.Ratio
import Data.Maybe
import Roguestar.Lib.FactionData
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import Roguestar.Lib.SpeciesData
import Roguestar.Lib.TerrainData

data Creature = Creature { creature_traits :: Map.Map CreatureTrait Integer,
                           creature_species :: Species,
                           creature_random_id :: Integer, -- random number attached to the creature, not unique
                           creature_damage :: Integer,
                           creature_faction :: Faction,
                           creature_points :: Integer }
                                deriving (Read,Show)

-- | Creature having no attributes and undefined 'creature_species', 'creature_random_id', and 'creature_faction'
--
empty_creature :: Creature
empty_creature = Creature {
    creature_traits = Map.empty,
    creature_species = error "empty_creature: undefined creature_species",
    creature_random_id = error "empty_creature: undefined creature_random_id",
    creature_damage = 0,
    creature_faction = error "empty_creature: undefined creature_faction",
    creature_points = 0 }

-- | Endomorphisms over a 'Creature'.  These are types that contribute some feature to a 'Creature',
-- so that 'Creature's can be defined concisely by those properties.
class CreatureEndo a where
    applyToCreature :: a -> Creature -> Creature

-- | Primitive numeric properties of a Creature.
class CreatureScore s where
    rawScore :: s -> Creature -> Integer

instance (CreatureEndo a,Integral i) => CreatureEndo (a,i) where
    applyToCreature (_,i) | i <= 0 = id
    applyToCreature (a,i) = applyToCreature (a,toInteger i - 1) . applyToCreature a

instance (CreatureEndo a) => CreatureEndo [a] where
    applyToCreature = appEndo . mconcat . map (Endo . applyToCreature)

data CreatureHealth = CreatureHealth {
    creature_absolute_health :: Integer,
    creature_absolute_damage :: Integer,
    creature_health :: Rational,
    creature_max_health :: Integer }

-- | The seven aptitudes.
data CreatureTrait =
     Aggression
   | Bulk
   | Caution
   | Dexterity
   | Fortitude
   | Perception
   | Speed
   | CharacterClass CharacterClass
         deriving (Eq,Read,Show,Ord)

instance CreatureEndo CreatureTrait where
    applyToCreature trait c = c { creature_traits = Map.insertWith (+) trait 1 $ creature_traits c }

instance CreatureScore CreatureTrait where
    rawScore trait c = fromMaybe 0 $ Map.lookup trait (creature_traits c)

-- | Combat modes:
-- Melee is armed close-quarters combat with bladed or blunt weapons
-- Ranged is combat with projectile weapons
-- Unarmed is close-quarters hand-to-hand
-- Splash represts diffuse damage caused by things like explosions or falling into lava.
data CreatureInteractionMode = Melee | Ranged | Unarmed | Splash
    deriving (Eq,Read,Show,Ord)

data CreatureAbility =
     ToughnessTrait
   | AttackSkill CreatureInteractionMode
   | DefenseSkill CreatureInteractionMode
   | DamageSkill CreatureInteractionMode
   | DamageReductionTrait CreatureInteractionMode
   | ReloadSkill CreatureInteractionMode
   | TerrainAffinity TerrainPatch
   | HideSkill
   | SpotSkill
   | JumpSkill
   | InventorySkill
         deriving (Eq,Read,Show,Ord)

instance CreatureEndo CharacterClass where
    applyToCreature character_class = applyToCreature (CharacterClass character_class)

instance CreatureScore CharacterClass where
    rawScore character_class = rawScore (CharacterClass character_class)

-- | Calculator to determine how many ranks a creature has in an ability.
-- Number of aptitude points plus n times number of ability points
figureAbility :: [CreatureTrait] -> Creature -> Integer
figureAbility traits c = round $ realToFrac x ** (1.0 / realToFrac (length traits))
    where x = product (map ((+1) . flip rawScore c) traits)

creatureAbilityScore :: CreatureAbility -> Creature -> Integer
creatureAbilityScore ToughnessTrait = figureAbility [Caution,Fortitude]
creatureAbilityScore (AttackSkill x) = figureAbility [Aggression,Dexterity]
creatureAbilityScore (DefenseSkill x) = figureAbility [Caution,Dexterity]
creatureAbilityScore (DamageSkill x) = figureAbility [Aggression,Bulk]
creatureAbilityScore (DamageReductionTrait x) = figureAbility [Caution,Bulk]
creatureAbilityScore (ReloadSkill x) = figureAbility [Aggression,Speed]
creatureAbilityScore (TerrainAffinity terrain_type) = figureAbility []
creatureAbilityScore HideSkill = figureAbility [Aggression,Perception]
creatureAbilityScore SpotSkill = figureAbility [Caution,Perception]
creatureAbilityScore JumpSkill = figureAbility [Speed]
creatureAbilityScore InventorySkill = figureAbility [Fortitude]

-- |
-- Answers the health/injury/maximum health of this creature.
creatureHealth :: Creature -> CreatureHealth
creatureHealth c = case () of
                       () | creature_max_health result <= 0 -> error "creatureHealth: creature_max_health <= 0"
                       () | otherwise -> result
    where result = CreatureHealth {
        creature_health = creature_absolute_health result % creature_max_health result,
        creature_absolute_health = creature_max_health result - creature_absolute_damage result,
        creature_absolute_damage = creature_damage c,
        creature_max_health = creatureAbilityScore ToughnessTrait c }