module Roguestar.Lib.Data.SpeciesTraits
    (SpeciesData(..),
     speciesInfo)
    where

import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.Data.SpeciesData

data SpeciesData = SpeciesData {
        species_traits :: [(MonsterTrait,Integer)],
        species_specials :: [MonsterSpecial] }

speciesInfo :: Species -> SpeciesData

speciesInfo RedRecreant = SpeciesData
    [(Aggression, 5),
     (Bulk,       1),
     (Caution,    4),
     (Dexterity,  4),
     (Fortitude,  2),
     (Perception, 5),
     (Speed,      10)]
    [Hover,
     Teleportation]
speciesInfo LavaLarva = SpeciesData
    [(Aggression, 20),
     (Bulk,       2),
     (Caution,    2),
     (Dexterity,  20),
     (Fortitude,  2),
     (Perception, 2),
     (Speed,      2)]
    []
speciesInfo Anachronid = SpeciesData
    [(Aggression, 3),
     (Bulk,       3),
     (Caution,    3),
     (Dexterity, 10),
     (Fortitude,  5),
     (Perception, 3),
     (Speed,      8)]
    [TemporalWeb]
speciesInfo TabularMonstrosity = SpeciesData
    [(Aggression, 1),
     (Bulk,      15),
     (Caution,    1),
     (Dexterity,  1),
     (Fortitude, 15),
     (Perception, 1),
     (Speed,      4)]
    [Hover,
     HolographicTrail]


