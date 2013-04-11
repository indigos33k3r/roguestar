--Data
module Roguestar.Lib.Data.MonsterData
    (Monster(..),
     MonsterTrait(..),
     MonsterSpecial(..),
     MonsterInteractionMode(..),
     MonsterAbility(..),
     MonsterEndo(..),
     MonsterScore(..),
     MonsterHealth(..),
     creatureHealth,
     creatureAbilityScore,
     empty_creature)
    where

import Roguestar.Lib.Data.PersistantData
import Data.Ratio
import Data.Maybe
import Roguestar.Lib.Data.FactionData
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List as List
import Roguestar.Lib.Data.SpeciesData
import Roguestar.Lib.Data.TerrainData

data Monster = Monster { creature_traits :: Map.Map MonsterTrait Integer,
                           creature_specials :: Set.Set MonsterSpecial,
                           creature_species :: Species,
                           creature_random_id :: Integer, -- random number attached to the creature, not unique
                           creature_damage :: Integer,
                           creature_faction :: Faction,
                           creature_points :: Integer }
                                deriving (Read,Show,Eq)

-- | Monster having no attributes and undefined 'creature_species', 'creature_random_id', and 'creature_faction'
--
empty_creature :: Monster
empty_creature = Monster {
    creature_traits = Map.empty,
    creature_specials = Set.empty,
    creature_species = error "empty_creature: undefined creature_species",
    creature_random_id = error "empty_creature: undefined creature_random_id",
    creature_damage = 0,
    creature_faction = error "empty_creature: undefined creature_faction",
    creature_points = 0 }

-- | Endomorphisms over a 'Monster'.  These are types that contribute some feature to a 'Monster',
-- so that 'Monster's can be defined concisely by those properties.
class MonsterEndo a where
    applyToMonster :: a -> Monster -> Monster

-- | Primitive numeric properties of a Monster.
class MonsterScore s where
    rawScore :: s -> Monster -> Integer

instance (MonsterEndo a,Integral i) => MonsterEndo (a,i) where
    applyToMonster (_,i) | i <= 0 = id
    applyToMonster (a,i) = applyToMonster (a,toInteger i - 1) . applyToMonster a

instance (MonsterEndo a) => MonsterEndo [a] where
    applyToMonster = appEndo . mconcat . map (Endo . applyToMonster)

data MonsterHealth = MonsterHealth {
    creature_absolute_health :: Integer,
    creature_absolute_damage :: Integer,
    creature_health :: Rational,
    creature_max_health :: Integer }

-- | The seven aptitudes.
data MonsterTrait =
     Aggression
   | Bulk
   | Caution
   | Dexterity
   | Fortitude
   | Perception
   | Speed
   | CharacterClass CharacterClass
         deriving (Eq,Read,Show,Ord)

instance MonsterEndo MonsterTrait where
    applyToMonster trait c = c { creature_traits = Map.insertWith (+) trait 1 $ creature_traits c }

instance MonsterScore MonsterTrait where
    rawScore trait c = fromMaybe 0 $ Map.lookup trait (creature_traits c)

data MonsterSpecial =
     Hover
   | Teleportation
   | TemporalWeb
   | ComplexificationMesh
         deriving (Eq,Read,Show,Ord)

instance MonsterEndo MonsterSpecial where
    applyToMonster special c = c { creature_specials = Set.insert special $ creature_specials c }

instance MonsterScore MonsterSpecial where
    rawScore special c = if Set.member special (creature_specials c) then 1 else 0

-- | Combat modes:
-- Melee is armed close-quarters combat with bladed or blunt weapons
-- Ranged is combat with projectile weapons
-- Unarmed is close-quarters hand-to-hand
-- Splash represts diffuse damage caused by things like explosions or falling into lava.
data MonsterInteractionMode = Melee | Ranged | Unarmed | Splash
    deriving (Eq,Read,Show,Ord)

data MonsterAbility =
     ToughnessTrait
   | AttackSkill MonsterInteractionMode
   | DefenseSkill MonsterInteractionMode
   | DamageSkill MonsterInteractionMode
   | DamageReductionTrait MonsterInteractionMode
   | TerrainAffinity Terrain
   | HideSkill
   | SpotSkill
   | JumpSkill
   | InventorySkill
         deriving (Eq,Read,Show,Ord)

instance MonsterEndo CharacterClass where
    applyToMonster character_class = applyToMonster (CharacterClass character_class)

instance MonsterScore CharacterClass where
    rawScore character_class = rawScore (CharacterClass character_class)

-- | Calculator to determine how many ranks a creature has in an ability.
-- Number of aptitude points plus n times number of ability points
figureAbility :: [MonsterTrait] -> Monster -> Integer
figureAbility []     _ = 1
figureAbility traits c = 1 + sum (map (flip rawScore c) traits) `div` List.genericLength traits

creatureAbilityScore :: MonsterAbility -> Monster -> Integer
creatureAbilityScore ToughnessTrait = figureAbility [Caution,Fortitude]
creatureAbilityScore (AttackSkill _) = figureAbility [Aggression,Dexterity]
creatureAbilityScore (DefenseSkill _) = figureAbility [Caution,Dexterity]
creatureAbilityScore (DamageSkill _) = figureAbility [Aggression,Bulk]
creatureAbilityScore (DamageReductionTrait _) = figureAbility [Caution,Bulk]
creatureAbilityScore (TerrainAffinity _) = figureAbility []
creatureAbilityScore HideSkill = figureAbility [Aggression,Perception]
creatureAbilityScore SpotSkill = figureAbility [Caution,Perception]
creatureAbilityScore JumpSkill = figureAbility [Speed]
creatureAbilityScore InventorySkill = figureAbility [Fortitude]

-- |
-- Answers the health/injury/maximum health of this creature.
creatureHealth :: Monster -> MonsterHealth
creatureHealth c = case () of
                       () | creature_max_health result <= 0 -> error "creatureHealth: creature_max_health <= 0"
                       () | otherwise -> result
    where result = MonsterHealth {
        creature_health = creature_absolute_health result % creature_max_health result,
        creature_absolute_health = creature_max_health result - creature_absolute_damage result,
        creature_absolute_damage = creature_damage c,
        creature_max_health = creatureAbilityScore ToughnessTrait c }
