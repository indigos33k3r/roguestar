module Roguestar.Lib.Model.Graph
    (Monster(..),
     Plane(..),
     Square(..))
    where

import qualified Data.Set as Set

data Monster a = Monster {
    monster_to_uid :: a,
    monster_to_square :: Square a }
        deriving (Show)

data Square a = Square {
    square_to_plane :: Plane a }
        deriving (Show)

data Plane a = Plane {
    plane_to_uid :: a,
    plane_to_monsters :: Set.Set (Monster a) }
        deriving (Show)

class HasGraphUID a where
    toUID :: a x -> x

instance HasGraphUID Monster where
    toUID = monster_to_uid

instance HasGraphUID Plane where
    toUID = plane_to_uid

eqByUID :: (HasGraphUID a, Eq x) => a x -> a x -> Bool
eqByUID a b = toUID a == toUID b

instance (Eq a) => Eq (Monster a) where
    (==) = eqByUID

instance (Eq a) => Eq (Plane a) where
    (==) = eqByUID

ordByUID :: (HasGraphUID a, Ord x) => a x -> a x -> Ordering
ordByUID a b = compare (toUID a) (toUID b)

instance (Ord a) => Ord (Monster a) where
    compare = ordByUID

instance (Ord a) => Ord (Plane a) where
    compare = ordByUID



