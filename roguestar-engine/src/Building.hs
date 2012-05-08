{-# LANGUAGE ScopedTypeVariables #-}

module Building
    (buildingSize,
     buildingShape,
     buildingBehavior,
     buildingSignal,
     activateFacingBuilding)
    where

import Prelude hiding (getContents)
import DB
import BuildingData
import Data.List
import Facing
import Data.Maybe
import Control.Monad.Maybe
import PlaneData
import Plane
import Position
import TerrainData
import Control.Monad.Error
import PowerUpData
import CharacterAdvancement
import DetailedLocation

-- | The total occupied surface area of a building.
buildingSize :: (DBReadable db) => BuildingRef -> db Integer
buildingSize = liftM (genericLength . buildingOccupies) . buildingShape

buildingShape :: (DBReadable db) => BuildingRef -> db BuildingShape
buildingShape building_ref =
    do constructed <- liftM fromLocation $ whereIs building_ref
       case constructed of
           Just building_shape -> return building_shape
           _ -> error "buildingType: impossible case"

buildingSignal :: (DBReadable db) => BuildingRef -> db (Maybe BuildingSignal)
buildingSignal  = liftM building_signal . dbGetBuilding

buildingBehavior :: (DBReadable db) => BuildingRef -> db BuildingBehavior
buildingBehavior building_ref = liftM building_behavior $ dbGetBuilding building_ref

deleteBuilding :: BuildingRef -> DB ()
deleteBuilding building_ref = dbUnsafeDeleteObject building_ref
                                                   (error "deleteBuilding: impossible case" :: forall m. DBReadable m => Reference () -> m Planar)

-- | Activate the facing building, returns True iff any building was actually activated.
activateFacingBuilding :: Facing -> CreatureRef -> DB Bool
activateFacingBuilding face creature_ref = liftM (fromMaybe False) $ runMaybeT $
    do (Parent plane_ref,position) <- MaybeT $ liftM fromLocation $ whereIs creature_ref
       buildings <- lift $ liftM mapLocations $ whatIsOccupying plane_ref $ offsetPosition (facingToRelative face) position
       liftM or $ lift $ forM buildings $ \(Child building_ref) ->
           do building_behavior <- buildingBehavior building_ref
              activateBuilding building_behavior creature_ref building_ref

activateBuilding :: BuildingBehavior -> CreatureRef -> BuildingRef -> DB Bool
activateBuilding (PowerUp pud) creature_ref building_ref =
    do captureNode pud creature_ref building_ref
       return True
activateBuilding (TwoWayStargate region) creature_ref building_ref =
    do (Parent plane_ref :: Parent Plane,Position (bx,by))
            <- liftM detail $ getPlanarLocation building_ref
       (Position (cx,cy)) <- liftM detail $ getPlanarLocation creature_ref
       case () of
           () | cy - by == (-1) ->
               do subsequent_plane <- maybe (throwError $ DBErrorFlag NoStargateAddress) return
                      =<< getSubsequent region plane_ref
                  portalCreatureTo (Just $ TwoWayStargate region) 1 creature_ref subsequent_plane
           () | cy - by == 1 ->
               do previous_plane <- maybe (throwError $ DBErrorFlag NoStargateAddress) (return . asParent)
                      =<< liftM fromLocation (whereIs plane_ref)
                  portalCreatureTo (Just $ TwoWayStargate region) (-1) creature_ref previous_plane
           () | otherwise ->
               do throwError $ DBErrorFlag BuildingApproachWrongAngle
       return True
activateBuilding (OneWayStargate region) creature_ref building_ref =
    do (Parent plane_ref :: Parent Plane,Position (bx,by))
            <- liftM detail $ getPlanarLocation building_ref
       (Position (cx,cy)) <- liftM detail $ getPlanarLocation creature_ref
       case () of
           () | cy - by == 1 ->
               do subsequent_plane <- maybe (throwError $ DBErrorFlag NoStargateAddress) return
                      =<< getSubsequent region plane_ref
                  portalCreatureTo Nothing 0 creature_ref subsequent_plane
           () | otherwise -> throwError $ DBErrorFlag BuildingApproachWrongAngle
       return True

-- | Deposit a creature in front of (-1) or behind (+1) a random portal on the specified plane.  Returns
-- the dbMove result from the action.
portalCreatureTo :: Maybe BuildingBehavior -> Integer -> CreatureRef -> PlaneRef -> DB (Location,Location)
portalCreatureTo building_behavior offset creature_ref plane_ref =
    do (all_buildings :: [BuildingRef]) <- liftM asChildren (getContents plane_ref)
       portals <- filterM (liftM ((== building_behavior) . Just) . buildingBehavior) all_buildings
       ideal_position <- if null portals
           then liftM2 (\x y -> Position (x,y)) (getRandomR (-40,40)) (getRandomR (-40,40))
           else do portal <- pickM portals
                   liftM (offsetPosition (0,offset) . detail) $ getPlanarLocation portal
       position <- pickRandomClearSite 1 0 0 ideal_position (not . (`elem` impassable_terrains)) plane_ref
       dbPushSnapshot $ TeleportEvent creature_ref
       move creature_ref $ Standing plane_ref position Here

captureNode :: PowerUpData -> CreatureRef -> BuildingRef -> DB ()
captureNode power_up_data creature_ref building_ref =
    do c <- dbGetCreature creature_ref
       let result = bumpCharacter power_up_data c
       dbModCreature (const $ character_new result) creature_ref
       deleteBuilding building_ref
       dbPushSnapshot $ BumpEvent {
           bump_event_creature = creature_ref,
           bump_event_new_level = newCharacterLevel result,
           bump_event_new_class = newCharacterClass result }

