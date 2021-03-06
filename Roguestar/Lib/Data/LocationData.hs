{-# LANGUAGE ScopedTypeVariables, PatternGuards, TypeFamilies #-}
--Data
module Roguestar.Lib.Data.LocationData
    (Reference,
     toUID,
     (=:=),
     (=/=),
     ReferenceType(..),
     genericReference,
     MonsterRef,
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

import Roguestar.Lib.Data.FacingData
import Roguestar.Lib.Data.ReferenceTypes
import Roguestar.Lib.Data.ToolData
import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.Data.PlaneData
import Roguestar.Lib.Data.BuildingData
import Control.Monad
import Roguestar.Lib.Position

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

-- | A convenience type to indicate that a reference is the parent component of a parent-child location record pair.
newtype Parent a = Parent { asParent :: Reference a }

-- | A convenience type to indicate that a reference is the child component of a parent-child location record pair.
newtype Child a = Child { asChild :: Reference a }

instance ReferenceType a => LocationDetail (Parent a) where
    fromLocation = fmap Parent . coerceReference . parentReference

instance ReferenceType a => LocationDetail (Child a) where
    fromLocation = fmap Child . coerceReference . childReference

class LocationConstructor l where
    type ChildTypeOf l :: *
    constructLocation :: Reference (ChildTypeOf l) -> l -> Maybe Location -> Location

instance LocationConstructor Standing where
    type ChildTypeOf Standing = MonsterData
    constructLocation a l _ = IsStanding a l

instance LocationConstructor Dropped where
    type ChildTypeOf Dropped = Tool
    constructLocation a l _ = IsDropped a l

instance LocationConstructor Inventory where
    type ChildTypeOf Inventory = Tool
    constructLocation a l _ = InInventory a l

instance LocationConstructor Wielded where
    type ChildTypeOf Wielded = Tool
    constructLocation a l _ = IsWielded a l

instance LocationConstructor Constructed where
    type ChildTypeOf Constructed = Building
    constructLocation a l _ = IsConstructed a l

instance LocationConstructor TheUniverse where
    type ChildTypeOf TheUniverse = PlaneData
    constructLocation a _ _ = InTheUniverse a

instance LocationConstructor Subsequent where
    type ChildTypeOf Subsequent = PlaneData
    constructLocation a l _ = IsSubsequent a l

instance LocationConstructor Beneath where
    type ChildTypeOf Beneath = PlaneData
    constructLocation a l _ = IsBeneath a l

returnToInventory :: Location -> Maybe Location
returnToInventory l | Just (Child tool,Parent creature) <- fromLocation l = Just $ InInventory tool (Inventory creature)
returnToInventory _ | otherwise = Nothing

shuntToTheUniverse :: Location -> Maybe Location
shuntToTheUniverse l | Just (Child plane) <- fromLocation l = Just $ InTheUniverse plane
shuntToTheUniverse _ | otherwise = Nothing

