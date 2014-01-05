{-# LANGUAGE FlexibleContexts #-}
module Roguestar.Lib.Graph.Classes
    (HasPlane(..),
     HasSquare(..),
     HasMonsters(..),
     HasMonster(..),
     comonsters,
     position,
     planeReference,
     monsterReference)
    where

import qualified Roguestar.Lib.Data.ReferenceTypes as References
import Roguestar.Lib.Position
import Roguestar.Lib.Graph.Graph
import qualified Data.Set as Set

class HasPlane a where
    plane :: a -> Plane

class HasMonsters a where
    monsters :: a -> Set.Set (Monster)

class HasMonster a where
    monster :: a -> Monster

class HasBuildings a where
    buildings :: a -> Set.Set (Building)

class HasSquare a where
    square :: a -> Square

instance HasPlane Plane where
    plane = id

instance HasPlane Square where
    plane = square_to_plane

instance HasPlane Monster where
    plane = plane . square

instance HasMonsters Plane where
    monsters = plane_to_monsters

instance HasMonsters Monster where
    monsters m = Set.singleton m

instance HasMonster Monster where
    monster = id

instance HasSquare Square where
    square = id

instance HasSquare Monster where
    square = monster_to_square

instance HasBuildings Plane where
    buildings = plane_to_buildings

-- | Monsters, other than this monster, on the same plane as this monster.
comonsters :: Monster -> Set.Set Monster
comonsters m = Set.filter (/= m) $ monsters $ plane m

position :: (HasSquare a) => a -> Position
position = square_to_position . square

planeReference :: (HasPlane a) => a -> References.PlaneRef
planeReference = plane_to_reference . plane

monsterReference :: (HasMonster a) => a -> References.MonsterRef
monsterReference = monster_to_reference . monster

