--Data
module Roguestar.Lib.DBPrivate
    (Reference(..),
     unsafeReference,
     toUID,
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
     CreatureRef,
     ToolRef,
     PlaneRef,
     BuildingRef)
    where

import Roguestar.Lib.HierarchicalDatabase
import Roguestar.Lib.Facing
import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.ToolData
import Roguestar.Lib.PlaneData
import Roguestar.Lib.Data.BuildingData
import Roguestar.Lib.PlanetData
import Roguestar.Lib.Position

--
-- For References and Locations we make considerable use of phantom types
-- to guarantee that such data structures are always consistent with the game logic,
-- e.g. a planet can not be wielded as a weapon.
--
-- DB and DBData import and re-export most of DBPrivate, but the un-exported portions
-- of this module are unsafe.  Other modules should not import DBPrivate.
--

-- |
-- Type representing the entire universe.
--
data TheUniverse = TheUniverse deriving (Read,Show,Eq,Ord)

type CreatureRef = Reference Creature
type ToolRef = Reference Tool
type PlaneRef = Reference Plane
type BuildingRef = Reference Building

-- |
-- A typesafe reference to any entity.
--
data Reference a = CreatureRef { uid:: Integer }
                 | PlaneRef { uid :: Integer }
                 | ToolRef { uid :: Integer }
                 | BuildingRef { uid :: Integer }
                 | UniverseRef
                       deriving (Eq,Ord,Read,Show)

unsafeReference :: Reference a -> Reference b
unsafeReference (CreatureRef x) = CreatureRef x
unsafeReference (PlaneRef x) = PlaneRef x
unsafeReference (ToolRef x) = ToolRef x
unsafeReference (BuildingRef x) = BuildingRef x
unsafeReference UniverseRef = UniverseRef

toUID :: Reference a -> Integer
toUID (UniverseRef) = 0
toUID a = uid a

-- |
-- The location of a Creature standing on a Plane.
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
    Inventory { inventory_creature :: CreatureRef }
    deriving (Read,Show,Eq,Ord)

-- |
-- The location of a weapon wielded in the hand of a creature.
--
data Wielded =
    Wielded { wielded_creature :: CreatureRef }
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
     IsStanding CreatureRef Standing
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

