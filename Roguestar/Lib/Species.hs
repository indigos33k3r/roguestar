
module Roguestar.Lib.Species
    (SpeciesData(..),
     speciesInfo)
    where

import Data.Char
import Roguestar.Lib.CreatureData
import Roguestar.Lib.SpeciesData
import Data.Monoid
import Roguestar.Lib.TerrainData

data SpeciesData = SpeciesData {
        species_traits :: [(CreatureTrait,Integer)] }

speciesInfo :: Species -> SpeciesData

speciesInfo RedRecreant = SpeciesData [(Aggression,3),(Bulk,3),(Caution,3),(Dexterity,3),(Fortitude,2),(Perception,12),(Speed,2)]
speciesInfo BlueRecreant = SpeciesData [(Aggression,3),(Bulk,6),(Caution,6),(Dexterity,3),(Fortitude,25),(Perception,3),(Speed,4)]
