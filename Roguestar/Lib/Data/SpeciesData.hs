module Roguestar.Lib.Data.SpeciesData
    (Species(..),
     all_species)
    where

data Species =
     LavaLarva
   | RedRecreant
   | Anachronid
   | TabularMonstrosity
       deriving (Eq,Ord,Bounded,Enum,Read,Show)

all_species :: [Species]
all_species = [minBound..maxBound]

