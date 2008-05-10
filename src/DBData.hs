{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

module DBData
    (Reference,
     toUID,
     CreatureRef,
     PlaneRef,
     ToolRef,
     TheUniverse(..),
     the_universe,
     (=:=),
     GenericReference(..),
     locationsOf,
     ReferenceType(..),
     LocationType(..),
     Location,
     Position(..),
     Standing(..),
     Dropped(..),
     Inventory(..),
     Wielded(..),
     _nullary,
     _creature,
     _tool,
     _plane,
     _standing,
     _dropped,
     _inventory,
     _wielded,
     _position,
     _facing,
     _the_universe,
     asLocationTyped,
     DBPrivate.S,
     location,
     entity,
     coerceReferenceTyped,
     isReferenceTyped,
     coerceLocationTyped,
     isLocationTyped,
     coerceEntityTyped,
     isEntityTyped,
     coerceLocationRecord,
     coerceLocation,
     coerceEntity,
     getLocation,
     getEntity,
     generalizeLocation,
     generalizeEntity,
     generalizeLocationRecord,
     toStanding,
     toDropped,
     toInventory,
     toWielded,
     returnToInventory)
    where

import Facing
import DBPrivate
import ToolData
import CreatureData
import PlaneData
import Data.Maybe
import Control.Monad
import Position

--
-- Type Instances
--
newtype Type a = Type a

_nullary :: Type (Reference ())
_nullary = Type $ error "_nullary: undefined"

_creature :: Type CreatureRef
_creature = Type $ error "_creature: undefined"

_tool :: Type ToolRef
_tool = Type $ error "_tool: undefined"

_plane :: Type PlaneRef
_plane = Type $ error "_plane: undefined"

_standing :: Type Standing
_standing = Type $ error "_standing: undefined"

_dropped :: Type Dropped
_dropped = Type $ error "_dropped: undefined"

_inventory :: Type Inventory
_inventory = Type $ error "_inventory: undefined"

_wielded :: Type Wielded
_wielded = Type $ error "_wielded: undefined"

_position :: Type Position
_position = Type $ error "_position: undefined"

_facing :: Type Facing
_facing = Type $ error "_facing: undefined"

_the_universe :: Type (Reference TheUniverse)
_the_universe = Type $ error "_the_universe: undefined"

--
-- Getting References generically.
--
class GenericReference a m | a -> m where
    fromLocation :: (ReferenceType x) => Location m (Reference x) b -> Maybe a
    generalizeReference :: a -> Reference ()

instance (ReferenceType a) => GenericReference (Reference a) m where
    fromLocation = coerceReference . entity
    generalizeReference = unsafeReference

instance (LocationType a,LocationType b) => GenericReference (Location m a b) m where
    fromLocation = coerceLocationRecord
    generalizeReference = getEntity

locationsOf :: (Monad m,LocationType a) => m [Location S (Reference ()) a] -> m [a]
locationsOf = liftM (map location)

--
-- Reference Equality
--
(=:=) :: (GenericReference a m,GenericReference b n) => a -> b -> Bool
a =:= b = generalizeReference a == generalizeReference b

--
-- References
--

the_universe :: Reference TheUniverse
the_universe = UniverseRef

coerceReferenceTyped :: (ReferenceType a) => Type (Reference a) -> Reference x -> Maybe (Reference a)
coerceReferenceTyped = const coerceReference

isReferenceTyped :: (ReferenceType a) => Type (Reference a) -> Reference x -> Bool
isReferenceTyped a = isJust . coerceReferenceTyped a

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

instance ReferenceType TheUniverse where
    coerceReference UniverseRef = Just UniverseRef
    coerceReference _ = Nothing

--
-- Locations
--
generalizeLocationRecord :: Location m e t -> Location m (Reference ()) ()
generalizeLocationRecord = unsafeLocation

generalizeLocation :: Location m e t -> Location m e ()
generalizeLocation = unsafeLocation

generalizeEntity :: Location m e t -> Location m (Reference ()) t
generalizeEntity = unsafeLocation

getLocation :: Location m e t -> Reference ()
getLocation (IsStanding _ s) = unsafeReference $ standing_plane s
getLocation (IsDropped _ d) = unsafeReference $ dropped_plane d
getLocation (InInventory _ c) = unsafeReference $ inventory_creature c
getLocation (IsWielded _ c) = unsafeReference $ wielded_creature c
getLocation (InTheUniverse _) = unsafeReference UniverseRef

getEntity :: Location m e t -> Reference ()
getEntity (IsStanding r _) = unsafeReference r
getEntity (IsDropped r _) = unsafeReference r
getEntity (InInventory r _) = unsafeReference r
getEntity (IsWielded r _) = unsafeReference r
getEntity (InTheUniverse r) = unsafeReference r

asLocationTyped :: (LocationType e,LocationType t) => Type e -> Type t -> Location m e t -> Location m e t
asLocationTyped _ _ = id

coerceLocationTyped :: (LocationType e,LocationType t) => Type t -> Location m e x -> Maybe (Location m e t)
coerceLocationTyped = const coerceLocation

isLocationTyped :: (LocationType e,LocationType t) => Type t -> Location m e x -> Bool
isLocationTyped t = isJust . coerceLocationTyped t

coerceEntityTyped :: (LocationType e,LocationType t) => Type e -> Location m x t -> Maybe (Location m e t)
coerceEntityTyped = const coerceEntity

isEntityTyped :: (LocationType e,LocationType t) => Type e -> Location m x t -> Bool
isEntityTyped t  = isJust . coerceEntityTyped t

coerceLocation :: (LocationType e,LocationType t) => Location m e x -> Maybe (Location m e t)
coerceLocation = coerceLocationRecord

coerceEntity :: (LocationType e,LocationType t) => Location m x t -> Maybe (Location m e t)
coerceEntity = coerceLocationRecord

coerceLocationRecord :: (LocationType e,LocationType t) => Location m x y -> Maybe (Location m e t)
coerceLocationRecord = fmap fst . coerceUnify
    where coerceUnify :: (LocationType e,LocationType t) => 
                             Location m x y -> Maybe (Location m e t,(e,t))
          coerceUnify l = do t <- extractLocation l
                             e <- extractEntity l
                             return (unsafeLocation l,(e,t))

location :: (LocationType t) => Location m e t -> t
location l = fromMaybe (error "location: type error") $ extractLocation l

entity :: (LocationType e) => Location m e t -> e
entity l = fromMaybe (error "entity: type error") $ extractEntity l

class (Eq a,Ord a) => LocationType a where
    extractLocation :: Location m e t -> Maybe a
    extractEntity :: Location m e t -> Maybe a

instance LocationType Standing where
    extractLocation (IsStanding _ s) = Just s
    extractLocation _ = Nothing
    extractEntity = const Nothing

instance LocationType Dropped where
    extractLocation (IsDropped _ d) = Just d
    extractLocation _ = Nothing
    extractEntity = const Nothing

instance LocationType Inventory where
    extractLocation (InInventory _ i) = Just i
    extractLocation _ = Nothing
    extractEntity = const Nothing

instance LocationType Wielded where
    extractLocation (IsWielded _ i) = Just i
    extractLocation _ = Nothing
    extractEntity = const Nothing

instance LocationType () where
    extractLocation = const $ Just ()
    extractEntity = const Nothing

instance LocationType Position where
    extractLocation (IsStanding _ s) = Just $ standing_position s
    extractLocation (IsDropped _ d) = Just $ dropped_position d
    extractLocation (InInventory {}) = Nothing
    extractLocation (IsWielded {}) = Nothing
    extractLocation (InTheUniverse {}) = Nothing
    extractEntity = const Nothing

instance LocationType Facing where
    extractLocation (IsStanding _ s) = Just $ standing_facing s
    extractLocation (IsDropped {}) = Nothing
    extractLocation (InInventory {}) = Nothing
    extractLocation (IsWielded {}) = Nothing
    extractLocation (InTheUniverse {}) = Nothing
    extractEntity = const Nothing

instance ReferenceType a => LocationType (Reference a) where
    extractLocation = coerceReference . getLocation
    extractEntity = coerceReference . getEntity

instance (LocationType a,LocationType b) => LocationType (a,b) where
    extractLocation l = liftM2 (,) (extractLocation l) (extractLocation l)
    extractEntity l = liftM2 (,) (extractEntity l) (extractEntity l)

--
-- Manipulating Locations
--
toStanding :: (LocationType t) => Standing -> Location m CreatureRef t -> Location m CreatureRef Standing
toStanding s l | isEntityTyped _creature l = IsStanding (entity l) s
toStanding _ _ = error "toStanding: type error"

toDropped :: (LocationType t) => Dropped -> Location m ToolRef t -> Location m ToolRef Dropped
toDropped d l | isEntityTyped _tool l = IsDropped (entity l) d
toDropped _ _ = error "toDropped: type error"

toInventory :: (LocationType t) => Inventory -> Location m ToolRef t -> Location m ToolRef Inventory
toInventory i l | isEntityTyped _tool l = InInventory (entity l) i
toInventory _ _ = error "toInventory: type error"

toWielded :: (LocationType t) => Wielded -> Location m ToolRef t -> Location m ToolRef Wielded
toWielded i l | isEntityTyped _tool l = IsWielded (entity l) i
toWielded _ _ = error "toWielded: type error"

returnToInventory :: Location m ToolRef Wielded -> Location m ToolRef Inventory
returnToInventory l = InInventory (entity l) (Inventory c)
    where Wielded c = location l
