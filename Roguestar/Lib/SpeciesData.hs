--Data
module Roguestar.Lib.SpeciesData
    (Species(..),
     all_species)
    where

--Data
data Species =
     BlueRecreant
   | RedRecreant
       deriving (Eq,Ord,Bounded,Enum,Read,Show)

all_species :: [Species]
all_species = [BlueRecreant,RedRecreant] -- [minBound..maxBound]

