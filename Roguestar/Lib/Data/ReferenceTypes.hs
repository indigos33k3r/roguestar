{-# LANGUAGE ScopedTypeVariables #-}

module Roguestar.Lib.Data.ReferenceTypes
    (Reference(..),
     unsafeReference,
     toUID,
     ReferenceType(..),
     (=:=),
     (=/=),
     Location(..),
     Position(..),
     Standing(..),
     Dropped(..),
     Inventory(..),
     Wielded(..),
     Constructed(..),
     TheUniverse(..),
     Subsequent(..),
     Beneath(..),
     MonsterRef,
     ToolRef,
     PlaneRef,
     BuildingRef)
    where

import Roguestar.Lib.Utility.HierarchicalDatabase
import Roguestar.Lib.Data.FacingData
import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.Data.ToolData
import Roguestar.Lib.Data.PlaneData
import Roguestar.Lib.Data.BuildingData
import Roguestar.Lib.Data.PlanetData
import Roguestar.Lib.Position

--
-- For References and Locations we make considerable use of phantom types
-- to guarantee that such data structures are always consistent with the game logic,
-- e.g. a planet can not be wielded as a weapon.
--

-- |
-- Type representing the entire universe.
--
data TheUniverse = TheUniverse deriving (Read,Show,Eq,Ord)

type MonsterRef = Reference MonsterData
type ToolRef = Reference Tool
type PlaneRef = Reference Plane
type BuildingRef = Reference Building

-- |
-- A typesafe reference to any entity.
--
data Reference a = MonsterRef { uid:: Integer }
                 | PlaneRef { uid :: Integer }
                 | ToolRef { uid :: Integer }
                 | BuildingRef { uid :: Integer }
                 | UniverseRef
                       deriving (Eq,Ord,Read,Show)

unsafeReference :: Reference a -> Reference b
unsafeReference (MonsterRef x) = MonsterRef x
unsafeReference (PlaneRef x) = PlaneRef x
unsafeReference (ToolRef x) = ToolRef x
unsafeReference (BuildingRef x) = BuildingRef x
unsafeReference UniverseRef = UniverseRef

toUID :: Reference a -> Integer
toUID (UniverseRef) = 0
toUID a = uid a

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

instance ReferenceType MonsterData where
    coerceReference (MonsterRef ref) = Just $ MonsterRef ref
    coerceReference _ = Nothing

instance ReferenceType Building where
    coerceReference (BuildingRef ref) = Just $ BuildingRef ref
    coerceReference _ = Nothing

instance ReferenceType TheUniverse where
    coerceReference UniverseRef = Just UniverseRef
    coerceReference _ = Nothing

instance (ReferenceType a, ReferenceType b) => ReferenceType (Either a b) where
    coerceReference x =
        let coerce_left :: Maybe (Reference a)
            coerce_left = coerceReference x
            coerce_right :: Maybe (Reference b)
            coerce_right = coerceReference x
            result = case (coerce_left,coerce_right) of
                (Just l,_) -> Just $ unsafeReference l
                (_,Just r) -> Just $ unsafeReference r
                _ -> Nothing
            in result

-- |
-- The location of a Monster standing on a Plane.
--
data Standing =
    Standing { standing_plane :: PlaneRef,
               standing_position :: Position,
               standing_facing :: Facing }
    deriving (Read,Show,Eq,Ord)

-- |
-- The location of a Tool dropped on a Plane.
--
data Dropped =
    Dropped { dropped_plane :: PlaneRef,
              dropped_position :: Position }
    deriving (Read,Show,Eq,Ord)

-- |
-- The location of a Building constructed on a Plane.
--
data Constructed =
    Constructed { constructed_plane :: PlaneRef,
                  constructed_position :: Position,
                  constructed_shape :: BuildingShape }
    deriving (Read,Show,Eq,Ord)

-- |
-- The location of a tool carried by a creature.
--
data Inventory =
    Inventory { inventory_creature :: MonsterRef }
    deriving (Read,Show,Eq,Ord)

-- |
-- The location of a weapon wielded in the hand of a creature.
--
data Wielded =
    Wielded { wielded_creature :: MonsterRef }
    deriving (Read,Show,Eq,Ord)

-- |
-- The location of a Plane linked to from another Plane, such as with a Stargate.
--
data Subsequent =
    Subsequent { subsequent_to :: PlaneRef,
                 subsequent_via :: PlanetRegion }
    deriving (Read,Show,Eq,Ord)

-- |
-- The location of a dungeon plane.
--
data Beneath =
    Beneath { beneath_of :: PlaneRef }
    deriving (Read,Show,Eq,Ord)

-- |
--
-- Represents a location.
--
-- Up to roguestar 0.6, Locations were typed.  As of 0.7 locations are untyped, but I added DetailedLocations.
--
data Location =
     IsStanding MonsterRef Standing
   | IsDropped ToolRef Dropped
   | InInventory ToolRef Inventory
   | IsWielded ToolRef Wielded
   | IsConstructed BuildingRef Constructed
   | InTheUniverse PlaneRef
   | IsSubsequent PlaneRef Subsequent
   | IsBeneath PlaneRef Beneath
    deriving (Read,Show,Eq)

instance HierarchicalRelation Location where
    parent (IsStanding _ t) = toUID $ standing_plane t
    parent (IsDropped _ t) = toUID $ dropped_plane t
    parent (InInventory _ t) = toUID $ inventory_creature t
    parent (IsWielded _ t) = toUID $ wielded_creature t
    parent (IsConstructed _ t) = toUID $ constructed_plane t
    parent (InTheUniverse _) = toUID UniverseRef
    parent (IsSubsequent _ t) = toUID $ subsequent_to t
    parent (IsBeneath _ t) = toUID $ beneath_of t
    child (IsStanding e _) = toUID e
    child (IsDropped e _) = toUID e
    child (InInventory e _) = toUID e
    child (IsWielded e _) = toUID e
    child (IsConstructed e _) = toUID e
    child (InTheUniverse e) = toUID e
    child (IsSubsequent e _) = toUID e
    child (IsBeneath e _) = toUID e

