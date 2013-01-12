--Data
module Roguestar.Lib.Species
    (SpeciesData(..),
     speciesInfo)
    where

--Data
import Roguestar.Lib.CreatureData
import Roguestar.Lib.SpeciesData

data SpeciesData = SpeciesData {
        species_traits :: [(CreatureTrait,Integer)],
        species_specials :: [CreatureSpecial] }

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
speciesInfo BlueRecreant = speciesInfo RedRecreant
