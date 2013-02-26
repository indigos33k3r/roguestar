--Data
module Roguestar.Lib.SpeciesData
    (Species(..),
     all_species)
    where

--Data
data Species =
     BlueRecreant
   | RedRecreant
   | Anachronid
   | TabularMonstrosity
       deriving (Eq,Ord,Bounded,Enum,Read,Show)

all_species :: [Species]
all_species = [minBound..maxBound]

