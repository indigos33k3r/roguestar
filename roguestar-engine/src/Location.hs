{-# LANGUAGE TypeFamilies, EmptyDataDecls, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Location
    (AbstractLocation,
     LocationView,
     Child(..),
     Parent(..),
     fromLocation,
     filterLocation,
     LocationProvides(..),
     abstractLocation,
     coerceLocation,
     Provided,
     Motion(..))
    where

import DBPrivate
import PlaneData
import CreatureData
import Data.Maybe
import Control.Monad
import Facing

newtype AbstractLocation a = AbstractLocation { concreteLocation :: (Location () ()) }

unsafeAbstractLocation :: AbstractLocation a -> AbstractLocation b
unsafeAbstractLocation = AbstractLocation . concreteLocation

abstractLocation :: Location a b -> AbstractLocation ()
abstractLocation = AbstractLocation . unsafeLocation

filterLocation :: (LocationView a,MonadPlus m) => (a -> Bool) -> m (AbstractLocation x) -> m (AbstractLocation a)
filterLocation f ls = 
    do l <- ls
       case locationView (unsafeAbstractLocation l) of
           Nothing -> mzero
           Just a -> if f a then return (unsafeAbstractLocation l) else mzero

{--------------------------------------------
-- LocationView
--------------------------------------------}

class LocationView a where
    locationView :: AbstractLocation () -> Maybe a

instance LocationView () where
    locationView _ = Just ()
    
instance LocationView (Location () ()) where
    locationView (AbstractLocation l) = Just $ unsafeLocation l

newtype Parent a = Parent { fromParent :: Reference a }

instance LocationView (Parent ()) where
    locationView l = case (fromLocation l :: Location () ()) of
        (IsStanding _ s) -> Just $ Parent $ unsafeReference $ standing_plane s
        (IsDropped _ d) -> Just $ Parent $ unsafeReference $ dropped_plane d
        (InInventory _ c) -> Just $ Parent $ unsafeReference $ inventory_creature c
        (IsWielded _ c) -> Just $ Parent $ unsafeReference $ wielded_creature c
        (IsConstructed _ c) -> Just $ Parent $ unsafeReference $ constructed_plane c
        (InTheUniverse _) -> Just $ Parent$ unsafeReference UniverseRef
        (IsSubsequent _ b) -> Just $ Parent $ unsafeReference $ subsequent_to b
        (IsBeneath _ b) -> Just $ Parent $ unsafeReference $ beneath_of b

instance LocationView (Parent TheUniverse) where
    locationView (AbstractLocation (InTheUniverse _)) = Just $ Parent $ UniverseRef

instance LocationView (Parent Plane) where
    locationView (AbstractLocation (IsSubsequent _ s)) = Just $ Parent $ subsequent_to s
    locationView (AbstractLocation (IsBeneath _ b)) = Just $ Parent $ beneath_of b
    locationView (AbstractLocation (IsDropped _ d)) = Just $ Parent $ dropped_plane d
    locationView (AbstractLocation (IsStanding _ s)) = Just $ Parent $ standing_plane s
    locationView (AbstractLocation (IsConstructed _ s)) = Just $ Parent $ constructed_plane s

newtype Child a = Child { fromChild :: Reference a }

instance LocationView (Child ()) where
    locationView l = case (fromLocation l :: Location () ()) of
        (IsStanding r _) -> Just $ Child $ unsafeReference r
        (IsDropped r _) -> Just $ Child $ unsafeReference r
        (InInventory r _) -> Just $ Child $ unsafeReference r
        (IsWielded r _) -> Just $ Child $ unsafeReference r
        (IsConstructed r _) -> Just $ Child $ unsafeReference r
        (InTheUniverse r) -> Just $ Child $ unsafeReference r
        (IsSubsequent r _) -> Just $ Child $ unsafeReference r
        (IsBeneath r _) -> Just $ Child $ unsafeReference r

instance LocationView (Child Plane) where
    locationView (AbstractLocation (IsSubsequent p _)) = Just $ Child p
    locationView (AbstractLocation (IsBeneath p _)) = Just $ Child p
    locationView (AbstractLocation (InTheUniverse p)) = Just $ Child p
    
instance LocationView (Child Creature) where
    locationView (AbstractLocation (IsStanding r _)) = Just $ Child $ unsafeReference r
    locationView (AbstractLocation (IsDropped {})) = Nothing
    locationView (AbstractLocation (InInventory {})) = Nothing
    locationView (AbstractLocation (IsWielded {})) = Nothing
    locationView (AbstractLocation (IsConstructed {})) = Nothing
    locationView (AbstractLocation (InTheUniverse {})) = Nothing
    locationView (AbstractLocation (IsSubsequent {})) = Nothing
    locationView (AbstractLocation (IsBeneath {})) = Nothing

instance LocationView Subsequent where
    locationView (AbstractLocation (IsSubsequent _ s)) = Just s
    locationView _ = Nothing

instance LocationView Beneath where
    locationView (AbstractLocation (IsBeneath _ b)) = Just b
    locationView _ = Nothing

instance LocationView Position where
    locationView (AbstractLocation (IsStanding _ s)) = Just $ standing_position s
    locationView (AbstractLocation (IsDropped _ d)) = Just $ dropped_position d
    locationView (AbstractLocation (InInventory {})) = Nothing
    locationView (AbstractLocation (IsWielded {})) = Nothing
    locationView (AbstractLocation (IsConstructed _ c)) = Just $ constructed_position c
    locationView (AbstractLocation (InTheUniverse {})) = Nothing
    locationView (AbstractLocation (IsSubsequent {})) = Nothing
    locationView (AbstractLocation (IsBeneath {})) = Nothing

instance LocationView Facing where
    locationView (AbstractLocation (IsStanding _ s)) = Just $ standing_facing s
    locationView (AbstractLocation (IsDropped _ d)) = Nothing
    locationView (AbstractLocation (InInventory {})) = Nothing
    locationView (AbstractLocation (IsWielded {})) = Nothing
    locationView (AbstractLocation (IsConstructed _ c)) = Nothing
    locationView (AbstractLocation (InTheUniverse {})) = Nothing
    locationView (AbstractLocation (IsSubsequent {})) = Nothing
    locationView (AbstractLocation (IsBeneath {})) = Nothing

instance (LocationView a,LocationView b) => LocationView (a,b) where
    locationView x =
        do a <- locationView x
           b <- locationView x
           return (a,b)

{--------------------------------------------
-- LocationProvides
--------------------------------------------}

data Provided

type family And a b :: *
type instance And Provided Provided = Provided

type family Or a b :: *
type instance Or Provided a = Provided
type instance Or a Provided = Provided

type family LocationProvides a b :: *

-- Nullary
type instance LocationProvides a () = Provided
type instance LocationProvides a (Location () ()) = Provided
type instance LocationProvides a (Parent ()) = Provided
type instance LocationProvides a (Child ()) = Provided
type instance LocationProvides a (b,c) =
    And (LocationProvides a b) (LocationProvides a c)

type instance LocationProvides Subsequent Subsequent = Provided
type instance LocationProvides Subsequent (Parent Plane) = Provided
type instance LocationProvides Subsequent (Child Plane) = Provided

type instance LocationProvides Beneath Beneath = Provided
type instance LocationProvides Beneath (Parent Plane) = Provided
type instance LocationProvides Beneath (Child Plane) = Provided

type instance LocationProvides (Child a) (Child a) = Provided
type instance LocationProvides (Parent a) (Parent a) = Provided

type instance LocationProvides (Child Creature) (Parent Plane) = Provided
type instance LocationProvides (Child Creature) Position = Provided
type instance LocationProvides (Child Creature) Facing = Provided

type instance LocationProvides (Parent Plane) Position = Provided
type instance LocationProvides (Parent Plane) Facing = Provided

fromLocation :: (LocationView b,LocationProvides a b ~ Provided) => AbstractLocation a -> b
fromLocation = fromMaybe (error "provide: impossible") . locationView . unsafeAbstractLocation

coerceLocation :: (LocationProvides a b ~ Provided) => AbstractLocation a -> AbstractLocation b
coerceLocation = unsafeAbstractLocation

{--------------------------------------------
-- LocationMove
--------------------------------------------}

class (LocationView (MoveFrom m), LocationView (MoveTo m)) => Motion m where
    type MoveFrom m :: *
    type MoveTo m :: *
    moveLocation :: m -> AbstractLocation (MoveFrom m) -> AbstractLocation (MoveTo m)

instance Motion TheUniverse where
    type MoveFrom TheUniverse = Child Plane
    type MoveTo TheUniverse = Parent TheUniverse
    moveLocation TheUniverse l = AbstractLocation $ InTheUniverse $ fromChild $ fromLocation l
