module CreatureAttribute
    (CreatureAttribute,
     CreatureAttributeGenerator,
     gender,
     CreatureAttribute.attributeStatic,
     CreatureAttribute.attributeMinMax,
     AG.attributeChoice,
     AG.attributeChoices,
     CreatureAttribute.generateAttributes,
     (&))
    where

import Data.Monoid
import AttributeGeneration as AG
import CreatureData
import Data.Ratio
import Control.Monad.Random
import FactionData
import SpeciesData

newtype CreatureAttribute = CreatureAttribute { fromCreatureAttribute :: Endo Creature }

instance CreatureEndo CreatureAttribute where
    applyToCreature (CreatureAttribute f) = appEndo f

(&) :: (CreatureEndo x,CreatureEndo y) => x -> y -> CreatureAttribute
x & y = CreatureAttribute $ Endo $ applyToCreature x . applyToCreature y

type CreatureAttributeGenerator = AttributeGenerator CreatureAttribute

-- |
-- Generate a ratio of males to females with any gender dimorphism.
-- 'gender (1%3) [attributeStatic 5 Speed] [attributeStatic 5 Mindfulness]' generates a
-- creature with a 1:2 male:female ratio, faster males, and more mindful females.
--
gender :: Rational -> [CreatureAttributeGenerator] -> [CreatureAttributeGenerator] -> CreatureAttributeGenerator
gender r male_dimorphism female_dimorphism = AG.attributeChoice r (CreatureAttribute.attributeStatic 1 Male:male_dimorphism) (CreatureAttribute.attributeStatic 1 Female:female_dimorphism)

attributeStatic :: (CreatureEndo a) => Integer -> a -> CreatureAttributeGenerator
attributeStatic n a = AG.attributeStatic n (CreatureAttribute $ Endo $ applyToCreature a)

attributeMinMax :: (CreatureEndo a) => (Integer,Integer) -> a -> CreatureAttributeGenerator
attributeMinMax min_max a = AG.attributeMinMax min_max (CreatureAttribute $ Endo $ applyToCreature a)

generateAttributes :: (MonadRandom m) => Faction -> Species -> CreatureAttributeGenerator -> m Creature
generateAttributes faction species_name attrib_generator =
    do attribs <- AG.generateAttributes attrib_generator
       random_id <- getRandomR (0,30000)
       let c = empty_creature {
           creature_species = species_name,
           creature_random_id = random_id,
           creature_faction = faction } 
       return $ (appEndo $ mconcat $ map fromCreatureAttribute attribs) c
