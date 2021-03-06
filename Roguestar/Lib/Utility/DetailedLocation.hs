{-# LANGUAGE TypeFamilies, EmptyDataDecls, ScopedTypeVariables, PatternGuards, FlexibleContexts #-}
--Core
module Roguestar.Lib.Utility.DetailedLocation
    (DetailedLocation,
     PlaneLocation,
     BuildingLocation,
     MonsterLocation,
     ToolLocation,
     CarriedLocation,
     PlanarLocation,
     filterLocations,
     mapLocations,
     asChildren,
     identityDetail,
     detail,
     Planar(..),
     LocationAssignmentTable)
    where

import Roguestar.Lib.Data.LocationData
import Roguestar.Lib.Data.PlaneData
import Roguestar.Lib.Data.BuildingData
import Roguestar.Lib.Data.ToolData
import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.Position
import Data.Maybe
import Control.Monad
import Roguestar.Lib.Data.FacingData

type PlaneLocation = DetailedLocation (Child PlaneData)
type BuildingLocation = DetailedLocation (Child Building)
type MonsterLocation = DetailedLocation (Child MonsterData)
type ToolLocation = DetailedLocation (Child Tool)
type CarriedLocation = DetailedLocation (Parent MonsterData)
type PlanarLocation = DetailedLocation Planar

data DetailedLocation a = DetailedLocation { dl_location :: Location }

instance LocationSource (DetailedLocation a) where
    toLocation = dl_location

instance (LocationDetail a) => LocationDetail (DetailedLocation a) where
    fromLocation source =
        do (_ :: a) <- fromLocation source
           return $ DetailedLocation source

filterLocations :: (LocationSource l, LocationDetail a) => (a -> Bool) -> [l] -> [DetailedLocation a]
filterLocations f = map DetailedLocation . filter (maybe False f . fromLocation) . map toLocation

mapLocations :: (LocationSource l, LocationDetail a) => [l] -> [a]
mapLocations = mapMaybe (fromLocation . toLocation)

identityDetail :: (LocationDetail a) => DetailedLocation a -> a
identityDetail = fromMaybe (error "identityDetail: impossible case: fromLocation call failed") . fromLocation . dl_location

detail :: (LocationDetail to,LocationAssignmentTable from to ~ Supported) => DetailedLocation from -> to
detail = fromMaybe (error "detail: impossible case: fromLocation call failed") . fromLocation . dl_location

asChildren :: (LocationSource l,LocationDetail (Child a)) => [l] -> [Reference a]
asChildren = map asChild . mapLocations

-- | A location with a parent plane and a multiposition.
-- That is, any physical object resting, walking, or constructed on a plane.
-- But not a Beneath or Subsequent plane.
data Planar = Planar {
        planar_parent :: PlaneRef,
        planar_position :: Position,
        planar_multiposition :: MultiPosition }

instance LocationDetail Planar where
    fromLocation l = liftM3 Planar (liftM (\(Parent x) -> x) $ fromLocation l) (fromLocation l) (fromLocation l)

instance LocationConstructor Planar where
    type ChildTypeOf Planar = ()
    constructLocation ref planar | Just creature_ref <- coerceReference ref =
                                   constructLocation creature_ref $ Standing (planar_parent planar) (planar_position planar) Here
    constructLocation ref planar | Just tool_ref     <- coerceReference ref =
                                   constructLocation tool_ref     $ Dropped (planar_parent planar) (planar_position planar)
    constructLocation ref planar | Just plane_ref    <- coerceReference ref =
                                   constructLocation plane_ref    $ Beneath (planar_parent planar)
    constructLocation ref planar | Just building_ref <- coerceReference ref =
                                   constructLocation building_ref $ Constructed (planar_parent planar)
                                                                                (planar_position planar)
                                                                                (error "LocationConstructor Planar: constructLocation: indeterminate")
    constructLocation _ _ | otherwise = error "LocationConstructor Planar - constructLocation: failed match"

-- | Meaning that an assignment from one location type to another is guaranteed to succeed.
data Supported

-- | This is not remotely a complete table, but will need to be added to on an as-needed basis.
type family LocationAssignmentTable   from             to                            :: *
type instance LocationAssignmentTable a                (DetailedLocation b)          = LocationAssignmentTable a b
type instance LocationAssignmentTable a                (Child ())                    = Supported
type instance LocationAssignmentTable a                (Parent ())                   = Supported
type instance LocationAssignmentTable Planar           (Parent PlaneData)                = Supported
type instance LocationAssignmentTable Planar           MultiPosition                 = Supported
type instance LocationAssignmentTable Planar           Position                      = Supported
type instance LocationAssignmentTable Planar           (Parent PlaneData, MultiPosition) = Supported
type instance LocationAssignmentTable Planar           (Parent PlaneData, Position)      = Supported
type instance LocationAssignmentTable (Child a)        (Child a)                     = Supported
type instance LocationAssignmentTable (Child MonsterData) Standing                       = Supported
type instance LocationAssignmentTable (Child MonsterData) (Parent PlaneData)                 = Supported
type instance LocationAssignmentTable (Child MonsterData) Position                       = Supported
type instance LocationAssignmentTable (Child MonsterData) MultiPosition                  = Supported
type instance LocationAssignmentTable (Child MonsterData) Planar                         = Supported
type instance LocationAssignmentTable (Child MonsterData) Facing                         = Supported
type instance LocationAssignmentTable (Child MonsterData) (Facing,Position)              = Supported
type instance LocationAssignmentTable (Child MonsterData) (Position,Facing)              = Supported
type instance LocationAssignmentTable (Child Building) (Parent PlaneData)                = Supported
type instance LocationAssignmentTable (Child Building) Position                      = Supported
type instance LocationAssignmentTable (Child Building) MultiPosition                 = Supported
type instance LocationAssignmentTable (Child Building) BuildingShape                 = Supported
type instance LocationAssignmentTable Beneath          (Child PlaneData)                 = Supported
type instance LocationAssignmentTable Subsequent       (Child PlaneData)                 = Supported
type instance LocationAssignmentTable Standing         Planar                        = Supported
type instance LocationAssignmentTable Standing         (Child MonsterData)               = Supported
type instance LocationAssignmentTable Standing         (Parent PlaneData)                = Supported
type instance LocationAssignmentTable Standing         Position                      = Supported
type instance LocationAssignmentTable Standing         MultiPosition                 = Supported
type instance LocationAssignmentTable Standing         Facing                        = Supported
type instance LocationAssignmentTable Wielded          (Child Tool)                  = Supported
type instance LocationAssignmentTable Dropped          (Child Tool)                  = Supported
type instance LocationAssignmentTable Inventory        (Child Tool)                  = Supported

