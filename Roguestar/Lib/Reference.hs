{-# LANGUAGE ScopedTypeVariables #-}

module Roguestar.Lib.Reference
    (ReferenceType(..),
     (=:=),
     (=/=))
    where

import Roguestar.Lib.DBPrivate
import Roguestar.Lib.PlaneData
import Roguestar.Lib.BuildingData
import Roguestar.Lib.ToolData
import Roguestar.Lib.CreatureData
import Data.Either
import Data.Maybe

--
-- Reference Equality
--
(=:=) :: Reference a -> Reference b -> Bool
a =:= b = toUID a == toUID b

(=/=) :: Reference a -> Reference b -> Bool
a =/= b = not $ a =:= b

class ReferenceType a where
    coerceReference :: Reference x -> Maybe (Reference a)

instance ReferenceType () where
    coerceReference = Just . unsafeReference

instance ReferenceType Plane where
    coerceReference (PlaneRef ref) = Just $ PlaneRef ref
    coerceReference _ = Nothing

instance ReferenceType Tool where
    coerceReference (ToolRef ref) = Just $ ToolRef ref
    coerceReference _ = Nothing

instance ReferenceType Creature where
    coerceReference (CreatureRef ref) = Just $ CreatureRef ref
    coerceReference _ = Nothing

instance ReferenceType Building where
    coerceReference (BuildingRef ref) = Just $ BuildingRef ref
    coerceReference _ = Nothing

instance ReferenceType TheUniverse where
    coerceReference UniverseRef = Just UniverseRef
    coerceReference _ = Nothing

instance (ReferenceType a, ReferenceType b) => ReferenceType (Either a b) where
    coerceReference x =
        let -- all of this monstrous let-binding is just to make the typecheck unambiguous
            bind :: Maybe (Reference x) -> x
            bind = undefined
            alike :: a -> a -> Bool
            alike _ _ = True
            coerce_left = coerceReference x
            coerce_right = coerceReference x
            bind_either = either (alike $ bind coerce_left)
                                 (alike $ bind coerce_right)
                                 (bind result)
            result = case (coerce_left,coerce_right) of
                (Just l,_) -> Just $ unsafeReference l
                (_,Just r) -> Just $ unsafeReference r
                _ -> Nothing
            in result
