{-# LANGUAGE ScopedTypeVariables, PatternGuards, TypeFamilies #-}
--Data
module Roguestar.Lib.DBData
    (Reference,
     toUID,
     genericReference,
     CreatureRef,
     PlaneRef,
     ToolRef,
     BuildingRef,
     TheUniverse(..),
     the_universe,
     parentReference,
     childReference,
     LocationSource(..),
     LocationDetail(..),
     Child(..),
     Parent(..),
     Location,
     Position(..),
     Standing(..),
     Dropped(..),
     Inventory(..),
     Wielded(..),
     Constructed(..),
     Subsequent(..),
     Beneath(..),
     LocationConstructor(..),
     returnToInventory,
     shuntToTheUniverse)
    where

import Roguestar.Lib.Facing
import Roguestar.Lib.DBPrivate
import Roguestar.Lib.ToolData
import Roguestar.Lib.CreatureData
import Roguestar.Lib.PlaneData
import Roguestar.Lib.Data.BuildingData
import Control.Monad
import Roguestar.Lib.Position
import Roguestar.Lib.Reference

--
-- Getting References generically.
--
class LocationSource a where
    toLocation :: a -> Location

instance LocationSource Location where
    toLocation = id

class LocationDetail a where
    fromLocation :: Location -> Maybe a

instance (LocationDetail a,LocationDetail b) => LocationDetail (Either a b) where
    fromLocation x = case (fromLocation x,fromLocation x) of
            (Just a,_) -> Just $ Left a
            (_,Just b) -> Just $ Right b
            _ | otherwise -> Nothing

instance (LocationDetail a,LocationDetail b) => LocationDetail ((,) a b) where
    fromLocation x = liftM2 (,) (fromLocation x) (fromLocation x)

instance LocationDetail Location where
    fromLocation = Just

--
-- References
--

the_universe :: Reference TheUniverse
the_universe = UniverseRef

genericReference :: Reference a -> Reference ()
genericReference = unsafeReference

--
-- Locations
--

parentReference :: Location -> Reference ()
parentReference (IsStanding _ s) = unsafeReference $ standing_plane s
parentReference (IsDropped _ d) = unsafeReference $ dropped_plane d
parentReference (InInventory _ c) = unsafeReference $ inventory_creature c
parentReference (IsWielded _ c) = unsafeReference $ wielded_creature c
parentReference (IsConstructed _ c) = unsafeReference $ constructed_plane c
parentReference (InTheUniverse _) = unsafeReference UniverseRef
parentReference (IsSubsequent _ b) = unsafeReference $ subsequent_to b
parentReference (IsBeneath _ b) = unsafeReference $ beneath_of b

childReference :: Location -> Reference ()
childReference (IsStanding r _) = unsafeReference r
childReference (IsDropped r _) = unsafeReference r
childReference (InInventory r _) = unsafeReference r
childReference (IsWielded r _) = unsafeReference r
childReference (IsConstructed r _) = unsafeReference r
childReference (InTheUniverse r) = unsafeReference r
childReference (IsSubsequent r _) = unsafeReference r
childReference (IsBeneath r _) = unsafeReference r

instance LocationDetail Standing where
    fromLocation (IsStanding _ s) = Just s
    fromLocation _ = Nothing

instance LocationDetail Dropped where
    fromLocation (IsDropped _ d) = Just d
    fromLocation _ = Nothing

instance LocationDetail Inventory where
    fromLocation (InInventory _ i) = Just i
    fromLocation _ = Nothing

instance LocationDetail Wielded where
    fromLocation (IsWielded _ i) = Just i
    fromLocation _ = Nothing

instance LocationDetail Constructed where
    fromLocation (IsConstructed _ i) = Just i
    fromLocation _ = Nothing

instance LocationDetail BuildingShape where
    fromLocation (IsConstructed _ c) = Just $ constructed_shape c
    fromLocation _ = Nothing

instance LocationDetail TheUniverse where
    fromLocation (InTheUniverse {}) = Just TheUniverse
    fromLocation _ = Nothing

instance LocationDetail Subsequent where
    fromLocation (IsSubsequent _ i) = Just i
    fromLocation _ = Nothing

instance LocationDetail Beneath where
    fromLocation (IsBeneath _ i) = Just i
    fromLocation _ = Nothing

instance LocationDetail Position where
    fromLocation (IsStanding _ s) = Just $ standing_position s
    fromLocation (IsDropped _ d) = Just $ dropped_position d
    fromLocation (InInventory {}) = Nothing
    fromLocation (IsWielded {}) = Nothing
    fromLocation (IsConstructed _ c) = Just $ constructed_position c
    fromLocation (InTheUniverse {}) = Nothing
    fromLocation (IsSubsequent {}) = Nothing
    fromLocation (IsBeneath {}) = Nothing

instance LocationDetail MultiPosition where
    fromLocation (IsConstructed _ c) = Just $ multiPosition (constructed_position c) (buildingOccupies $ constructed_shape c)
    fromLocation x = fmap (toMultiPosition :: Position -> MultiPosition) $ fromLocation x

instance LocationDetail Facing where
    fromLocation (IsStanding _ s) = Just $ standing_facing s
    fromLocation (IsDropped {}) = Nothing
    fromLocation (InInventory {}) = Nothing
    fromLocation (IsWielded {}) = Nothing
    fromLocation (IsConstructed {}) = Nothing
    fromLocation (InTheUniverse {}) = Nothing
    fromLocation (IsSubsequent {}) = Nothing
    fromLocation (IsBeneath {}) = Nothing

newtype Parent a = Parent { asParent :: Reference a }
newtype Child a = Child { asChild :: Reference a }

instance ReferenceType a => LocationDetail (Parent a) where
    fromLocation = fmap Parent . coerceReference . parentReference

instance ReferenceType a => LocationDetail (Child a) where
    fromLocation = fmap Child . coerceReference . childReference

class LocationConstructor l where
    type ReferenceTypeOf l :: *
    constructLocation :: Reference (ReferenceTypeOf l) -> l -> Maybe Location -> Location

instance LocationConstructor Standing where
    type ReferenceTypeOf Standing = Creature
    constructLocation a l _ = IsStanding a l

instance LocationConstructor Dropped where
    type ReferenceTypeOf Dropped = Tool
    constructLocation a l _ = IsDropped a l

instance LocationConstructor Inventory where
    type ReferenceTypeOf Inventory = Tool
    constructLocation a l _ = InInventory a l

instance LocationConstructor Wielded where
    type ReferenceTypeOf Wielded = Tool
    constructLocation a l _ = IsWielded a l

instance LocationConstructor Constructed where
    type ReferenceTypeOf Constructed = Building
    constructLocation a l _ = IsConstructed a l

instance LocationConstructor TheUniverse where
    type ReferenceTypeOf TheUniverse = Plane
    constructLocation a _ _ = InTheUniverse a

instance LocationConstructor Subsequent where
    type ReferenceTypeOf Subsequent = Plane
    constructLocation a l _ = IsSubsequent a l

instance LocationConstructor Beneath where
    type ReferenceTypeOf Beneath = Plane
    constructLocation a l _ = IsBeneath a l

returnToInventory :: Location -> Maybe Location
returnToInventory l | Just (Child tool,Parent creature) <- fromLocation l = Just $ InInventory tool (Inventory creature)
returnToInventory _ | otherwise = Nothing

shuntToTheUniverse :: Location -> Maybe Location
shuntToTheUniverse l | Just (Child plane) <- fromLocation l = Just $ InTheUniverse plane
shuntToTheUniverse _ | otherwise = Nothing

