{-# LANGUAGE ScopedTypeVariables #-}
--Core
module Roguestar.Lib.Core.Building
    (buildingSize,
     buildingShape,
     buildingBehavior,
     buildingSignal,
     activateFacingBuilding)
    where

import Prelude hiding (getContents)
import Roguestar.Lib.DB
import Roguestar.Lib.Data.BuildingData
import Data.List
import Roguestar.Lib.Data.FacingData
import Data.Maybe
import Control.Monad.Maybe
import Control.Monad.Random
import Roguestar.Lib.PlaneData
import Roguestar.Lib.Core.Plane
import Roguestar.Lib.Position
import Roguestar.Lib.TerrainData
import Control.Monad.Error
import Roguestar.Lib.PowerUpData
import Roguestar.Lib.Behavior.CharacterAdvancement
import Roguestar.Lib.Utility.DetailedLocation
import Roguestar.Lib.Data.PlayerState

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
activateFacingBuilding :: Facing -> MonsterRef -> DB Bool
activateFacingBuilding face creature_ref = liftM (fromMaybe False) $ runMaybeT $
    do (Parent plane_ref,position) <- MaybeT $ liftM fromLocation $ whereIs creature_ref
       buildings <- lift $ liftM mapLocations $ whatIsOccupying plane_ref $ offsetPosition (facingToRelative face) position
       liftM or $ lift $ forM buildings $ \(Child building_ref) ->
           do building_behavior_type <- buildingBehavior building_ref
              activateBuilding building_behavior_type creature_ref building_ref

activateBuilding :: BuildingBehavior -> MonsterRef -> BuildingRef -> DB Bool
activateBuilding (PowerUp pud) creature_ref building_ref =
    do captureNode pud creature_ref building_ref
       return True
activateBuilding (TwoWayStargate _) creature_ref building_ref =
    do (Parent _ :: Parent Plane,building_position :: Position) <- liftM detail $ getPlanarLocation building_ref
       (creature_position :: Position) <- liftM detail $ getPlanarLocation creature_ref
       case () of
           () | distanceBetweenChessboard creature_position building_position == 1 ->
               do setPlayerState $ GameOver PlayerIsVictorious
           () | otherwise ->
               do throwError $ DBErrorFlag BuildingApproachWrongAngle
       return True
activateBuilding (OneWayStargate region) creature_ref building_ref =
    do (Parent plane_ref :: Parent Plane,Position (_,by))
            <- liftM detail $ getPlanarLocation building_ref
       (Position (_,cy)) <- liftM detail $ getPlanarLocation creature_ref
       _ <- case () of
           () | cy - by == 1 ->
               do subsequent_plane <- maybe (throwError $ DBErrorFlag NoStargateAddress) return
                      =<< getSubsequent region plane_ref
                  portalMonsterTo Nothing 0 creature_ref subsequent_plane
           () | otherwise -> throwError $ DBErrorFlag BuildingApproachWrongAngle
       return True

-- | Deposit a creature in front of (-1) or behind (+1) a random portal on the specified plane.  Returns
-- the dbMove result from the action.
portalMonsterTo :: Maybe BuildingBehavior -> Integer -> MonsterRef -> PlaneRef -> DB (Location,Location)
portalMonsterTo building_behavior_type offset creature_ref plane_ref =
    do (all_buildings :: [BuildingRef]) <- liftM asChildren (getContents plane_ref)
       portals <- filterM (liftM ((== building_behavior_type) . Just) . buildingBehavior) all_buildings
       ideal_position <- if null portals
           then liftM2 (\x y -> Position (x,y)) (getRandomR (-40,40)) (getRandomR (-40,40))
           else do portal <- weightedPickM $ unweightedSet portals
                   liftM (offsetPosition (0,offset) . detail) $ getPlanarLocation portal
       position <- pickRandomClearSite 1 0 0 ideal_position (not . (`elem` impassable_terrains)) plane_ref
       dbPushSnapshot $ TeleportEvent creature_ref
       move creature_ref $ Standing plane_ref position Here

captureNode :: PowerUpData -> MonsterRef -> BuildingRef -> DB ()
captureNode power_up_data creature_ref building_ref =
    do c <- dbGetMonster creature_ref
       let result = bumpCharacter power_up_data c
       dbModMonster (const $ character_new result) creature_ref
       deleteBuilding building_ref
       dbPushSnapshot $ BumpEvent {
           bump_event_creature = creature_ref,
           bump_event_new_level = newCharacterLevel result,
           bump_event_new_class = newCharacterClass result }

