{-# LANGUAGE FlexibleContexts #-}
module Roguestar.Lib.Model.Classes
    (HasPlane(..),
     HasMonsters(..),
     comonsters)
    where

import Control.Arrow
import Roguestar.Lib.Model.Graph
import qualified Data.Set as Set

class HasPlane a where
    plane :: a x -> Plane x

class HasMonsters a where
    monsters :: a x -> Set.Set (Monster x)

instance HasPlane Plane where
    plane = id

instance HasPlane Square where
    plane = square_to_plane

instance HasPlane Monster where
    plane = monster_to_square >>> square_to_plane

instance HasMonsters Plane where
    monsters = plane_to_monsters

instance HasMonsters Monster where
    monsters m = Set.singleton m

-- | Monsters, other than this monster, on the same plane as this monster.
comonsters :: (Eq (Monster x)) => Monster x -> Set.Set (Monster x)
comonsters m = Set.filter (/= m) $ monsters $ plane m

