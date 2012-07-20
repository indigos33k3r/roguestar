
module Roguestar.Lib.Species
    (SpeciesData(..),
     speciesInfo)
    where

import Data.Char
import Roguestar.Lib.CreatureData
import Roguestar.Lib.SpeciesData
import Roguestar.Lib.CreatureAttribute
import Data.Monoid
import Roguestar.Lib.TerrainData

data SpeciesData = SpeciesData {
        species_recurring_attributes :: CreatureAttribute,
        species_starting_attributes :: [CreatureAttributeGenerator] }

-- | Give a minimum and maximum ability score, along with a list of special aptitudes that are doubled.
aptitudeBlock :: Integer -> Integer -> [CreatureAptitude] -> CreatureAttributeGenerator
aptitudeBlock minimal maximal special = mconcat $
    map (\a -> attributeMinMax (minimal,maximal) a) [minBound..maxBound :: CreatureAptitude] ++
    map (\a -> attributeMinMax (minimal,maximal) a) special

-- | Low probability, large magnitude bonuses to aptitude scores.
surpriseAptitudes :: CreatureAttributeGenerator
surpriseAptitudes = mconcat $ map (\a -> attributeChoice 0.05 [attributeMinMax (1,30) a] []) [minBound..maxBound :: CreatureAptitude]

speciesInfo :: Species -> SpeciesData

speciesInfo Anachronid = SpeciesData (Speed & Mindfulness & SpotSkill) [
    gender 0.0,
    aptitudeBlock 10 25 [Speed,Mindfulness],
    attributeStatic 15 SpotSkill,
    surpriseAptitudes]

speciesInfo Androsynth = SpeciesData (Strength & Intellect) [
    aptitudeBlock 12 17 [Strength,Intellect]]

speciesInfo Ascendant = SpeciesData (Strength & Mindfulness) [
    gender 0.5,
    aptitudeBlock 5 15 [Strength,Mindfulness],
    surpriseAptitudes,
    attributeStatic 10 JumpSkill]

speciesInfo Caduceator = SpeciesData (Strength & Charisma) [
    gender 0.5,
    aptitudeBlock 5 15 [Strength,Charisma],
    surpriseAptitudes]

speciesInfo DustVortex = SpeciesData (Speed & Mindfulness) [
    aptitudeBlock 3 5 [Speed,Mindfulness],
    attributeStatic 10 JumpSkill]

speciesInfo Encephalon = SpeciesData (Constitution & Intellect) [
    gender 0.5,
    aptitudeBlock 3 20 [Constitution,Intellect]]

speciesInfo Hellion = SpeciesData (Strength & Perception) [
    gender 0.5,
    aptitudeBlock 5 15 [Strength,Perception],
    surpriseAptitudes,
    attributeStatic 5 $ HideSkill]

speciesInfo Goliath = SpeciesData (Constitution & Perception) [
    gender 0.5,
    aptitudeBlock 3 20 [Constitution,Perception],
    surpriseAptitudes,
    attributeStatic 4 $ DamageReductionTrait Melee,
    attributeStatic 4 $ DamageReductionTrait Ranged,
    attributeStatic 4 $ DamageReductionTrait Unarmed]

speciesInfo Kraken = SpeciesData (Constitution & Charisma) [
    gender 0.5,
    aptitudeBlock 3 20 [Constitution,Charisma],
    attributeStatic 1 $ TerrainAffinity Water,
    surpriseAptitudes]

speciesInfo Myrmidon = SpeciesData (Speed & Intellect) [
    gender 0.0,
    aptitudeBlock 5 15 [Speed,Intellect],
    surpriseAptitudes,
    attributeStatic 5 $ AttackSkill Melee,
    attributeStatic 5 $ DefenseSkill Melee]

speciesInfo Perennial = SpeciesData (Constitution & Mindfulness) [
    aptitudeBlock 1 25 [Constitution, Mindfulness],
    attributeStatic 1 $ TerrainAffinity Forest,
    attributeStatic 1 $ TerrainAffinity DeepForest,
    surpriseAptitudes]

speciesInfo Recreant = SpeciesData (Speed & Perception) [
    aptitudeBlock 2 5 [Speed,Perception],
    surpriseAptitudes, surpriseAptitudes,
    attributeStatic 5 $ AttackSkill Ranged,
    attributeStatic 5 $ DamageSkill Ranged]

speciesInfo Reptilian = SpeciesData (Speed & Charisma) [
    gender 0.5,
    aptitudeBlock 5 15 [Speed,Charisma],
    surpriseAptitudes,
    attributeStatic 5 $ AttackSkill Unarmed,
    attributeStatic 5 $ DefenseSkill Unarmed]

