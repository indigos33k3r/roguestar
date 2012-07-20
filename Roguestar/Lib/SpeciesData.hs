module Roguestar.Lib.SpeciesData
    (Species(..),
     all_species)
    where

data Species =
     Anachronid
   | Androsynth
   | Ascendant
   | Caduceator
   | DustVortex
   | Encephalon
   | Goliath
   | Hellion
   | Kraken
   | Myrmidon
   | Perennial
   | Recreant
   | Reptilian
       deriving (Eq,Ord,Bounded,Enum,Read,Show)

all_species :: [Species]
all_species = [Recreant] -- [minBound..maxBound]

