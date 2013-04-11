{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
--Core
module Roguestar.Lib.Tool
    (pickupTool,
     wieldTool,
     dropTool,
     availablePickups,
     availableWields,
     getWielded,
     deleteTool,
     toolValue)
    where

import Prelude hiding (getContents)
import Roguestar.Lib.DB
import Roguestar.Lib.Reference
import Roguestar.Lib.Utility.DetailedLocation
import Control.Monad.Error
import Data.Maybe
import Data.List as List
import Roguestar.Lib.Data.ToolData
import Roguestar.Lib.Substances
import Roguestar.Lib.Core.Plane
import Roguestar.Lib.Data.PlaneData

pickupTool :: (DBReadable db) =>
                MonsterRef ->
                ToolRef ->
                db (Inventory)
pickupTool creature_ref tool_ref =
    do creature_loc <- whereIs creature_ref
       tool_loc <- whereIs tool_ref
       distance_between <- distanceBetweenSquared creature_ref tool_ref
       when (parentReference tool_loc =/= parentReference creature_loc || distance_between /= Just 0) $
           throwError (DBErrorFlag ToolIs_NotAtFeet)
       return $ Inventory creature_ref

-- | Move a tool into wielded position for whatever creature is carrying or standing over it.
wieldTool :: (DBReadable db) => ToolRef -> db Wielded
wieldTool tool_ref =
    do l <- whereIs tool_ref
       case () of
           () | Just l' <- fromLocation l -> return l' -- if it coerces into our return type, then it's already wielded
           () | Just (Dropped plane_ref position) <- fromLocation l ->
               do pickupers <- liftM (mapLocations . filterLocations (== position)) $ getContents plane_ref
                  case pickupers of -- the creature that is standing over the tool -- there can be only one
                      [Child single_pickuper] -> return $ Wielded single_pickuper
                      [] -> throwError $ DBErrorFlag ToolIs_Unreachable
                      _ -> throwError $ DBError "dbWieldTool: there were multiple creatures in reach of a single tool"
           () | Just (Inventory c) <- fromLocation l -> return $ Wielded c
           () | otherwise -> throwError $ DBErrorFlag ToolIs_NotWieldable

dropTool :: (DBReadable db) => ToolRef -> db Dropped
dropTool tool_ref =
    do tool_location <- liftM identityDetail $ getPlanarLocation tool_ref
       return $ Dropped (planar_parent tool_location) (planar_position tool_location)

availablePickups :: (DBReadable db) => MonsterRef -> db [ToolRef]
availablePickups creature_ref =
    do (Parent plane_ref :: Parent Plane, creature_position :: Position) <- liftM detail $ getPlanarLocation creature_ref
       pickups <- liftM (mapLocations . filterLocations (==creature_position)) $ getContents plane_ref
       return $ List.map (asChild . identityDetail) pickups

-- | List of tools that the specified creature may choose to wield.
-- That is, they are either on the ground or in the creature's inventory.
availableWields :: (DBReadable db) => MonsterRef -> db [ToolRef]
availableWields creature_ref =
    do carried_tools :: [ToolRef] <- liftM (List.map (asChild . identityDetail) . mapLocations) $ getContents creature_ref
       pickups <- availablePickups creature_ref
       return $ List.union carried_tools pickups

getWielded :: (DBReadable db) => MonsterRef -> db (Maybe ToolRef)
getWielded = liftM (listToMaybe . List.map (asChild . detail) . filterLocations (\(Wielded {}) -> True)) . getContents

-- | Safely delete tools.
deleteTool :: ToolRef -> DB ()
deleteTool tool_ref = dbUnsafeDeleteObject tool_ref $
    (error "deleteTool: impossible case: tools shouldn't contain anything" :: forall m. (DBReadable m) => Reference () -> m Planar)

toolValue :: (DBReadable db) => ToolRef -> db Integer
toolValue tool_ref =
    do t <- dbGetTool tool_ref
       return $ case t of
          DeviceTool _ d -> deviceValue d
          Sphere substance -> substanceValue substance

