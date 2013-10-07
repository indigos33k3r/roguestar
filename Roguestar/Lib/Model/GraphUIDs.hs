module Roguestar.Lib.Model.GraphUIDs
    ()
    where

class HasGraphUID a where
    toUID :: a x -> x

instance HasGraphUID Monster where
    toUID = monster_to_uid

instance HasGraphUID Plane where
    toUID = plane_to_uid

eqByUID :: (HasGraphUID a, Eq x) => a x -> a x -> Bool
eqByUID a b = toUID a == toUID b

