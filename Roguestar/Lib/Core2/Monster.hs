module Roguestar.Lib.Core2.Monster
    (comonsters,
     enemies,
     isEnemy)
    where

import Roguestar.Lib.Graph
import Roguestar.Lib.Data.FactionData
import qualified Data.Set as Set

-- | Monsters, other than this monster, on the same plane as this monster.
comonsters :: Monster -> Set.Set Monster
comonsters m = Set.filter (/= m) $ monsters $ plane m

-- | All enemies of this monster, on the same plane as this monster.
enemies :: Monster -> Set.Set Monster
enemies me = Set.filter (isEnemy me) $ comonsters me

-- | True if two monsters are from enemy factions.
isEnemy :: Monster -> Monster -> Bool
isEnemy m1 m2 = (getFaction m1 /= getFaction m2)
