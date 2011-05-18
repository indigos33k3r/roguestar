{-# LANGUAGE Arrows, OverloadedStrings, TypeFamilies, FlexibleContexts, RankNTypes #-}

module AnimationBuildings
    (buildingAvatar)
    where

import RSAGL.FRP
import RSAGL.Math
import RSAGL.Animation
import RSAGL.Color.RSAGLColors
import Animation
import VisibleObject
import Models.LibraryData
import Control.Arrow
import Scene

type BuildingAvatarSwitch m = AvatarSwitch () () m
type BuildingAvatar e m = FRP e (BuildingAvatarSwitch m) () ()

-- | An avatar for a building.  This function
-- detects the type of a building based on the
-- FRP Thread ID, and switches to the appropriate
-- type of building avatar.
buildingAvatar :: (FRPModel m) => BuildingAvatar e m
buildingAvatar = proc () ->
    do objectTypeGuard (== "building") -< ()
       m_building_type <- objectDetailsLookup ThisObject "building-type" -< ()
       switchContinue -< (fmap switchTo m_building_type,())
       returnA -< ()
  where switchTo "monolith" = simpleBuildingAvatar Monolith
        switchTo "anchor" = planetaryAnchorAvatar
        switchTo "portal" = simpleBuildingAvatar Portal
        switchTo "cybergate" = cybergateBuildingAvatar
        switchTo _ = questionMarkAvatar >>> arr (const ())

simpleBuildingAvatar :: (FRPModel m, LibraryModelSource lm) =>
                        lm -> BuildingAvatar e m
simpleBuildingAvatar building_model = genericBuildingAvatar $ proc () ->
    do libraryA -< (scene_layer_local,building_model)
       returnA -< ()

genericBuildingAvatar :: (FRPModel m) =>
                         (forall x y. FRP e (FRP1Context x y (BuildingAvatarSwitch m)) () ()) ->
                         BuildingAvatar e m
genericBuildingAvatar actionA = proc () ->
    do visibleObjectHeader -< ()
       m_orientation <- objectIdealOrientation ThisObject -< ()
       whenJust (transformA actionA) -< fmap
           (\o -> (o,())) m_orientation
       returnA -< ()

cybergateBuildingAvatar :: (FRPModel m) =>
                           BuildingAvatar e m
cybergateBuildingAvatar = genericBuildingAvatar $ proc () ->
    do transformA libraryA -< (affineOf $ translate (Vector3D 0 (-0.5) 0),
                              (scene_layer_local,SimpleModel Cybergate))
       transformA libraryA -< (affineOf $ translate (Vector3D (-1) (-1) 0) . scale (Vector3D 1 1 1.5),
                               (scene_layer_local,SimpleModel Cyberpylon))
       transformA libraryA -< (affineOf $ translate (Vector3D (-2) (-2) 0) . scale (Vector3D 1 1 1),
                               (scene_layer_local,SimpleModel Cyberpylon))
       transformA libraryA -< (affineOf $ translate (Vector3D (-3) (-3) 0) . scale (Vector3D 1 1 0.5),
                               (scene_layer_local,SimpleModel Cyberpylon))
       transformA libraryA -< (affineOf $ translate (Vector3D 1 (-1) 0) . scale (Vector3D 1 1 1.5),
                               (scene_layer_local,SimpleModel Cyberpylon))
       transformA libraryA -< (affineOf $ translate (Vector3D 2 (-2) 0) . scale (Vector3D 1 1 1),
                               (scene_layer_local,SimpleModel Cyberpylon))
       transformA libraryA -< (affineOf $ translate (Vector3D 3 (-3) 0) . scale (Vector3D 1 1 0.5),
                               (scene_layer_local,SimpleModel Cyberpylon))
       lightningBolt -< (Green, Point3D (-3) (-3) 0.5,Point3D (-2) (-2) 1.0)
       lightningBolt -< (Green, Point3D (-2) (-2) 1.0,Point3D (-1) (-1) 1.5)
       lightningBolt -< (Green, Point3D (3) (-3) 0.5,Point3D (2) (-2) 1.0)
       lightningBolt -< (Green, Point3D (2) (-2) 0.5,Point3D (1) (-1) 1.5)
       random_height <- randomA -< (-0.5,0.99 :: RSdouble)
       let width = sqrt $ 1.0 - random_height^2
       lightningBolt -< (Green, Point3D (-1) (-1) 0.5,Point3D (-width) (-0.5) (random_height*1.5+1.5))
       lightningBolt -< (Green, Point3D (1) (-1) 0.5, Point3D width (-0.5) (random_height*1.5+1.5))

lightningBolt :: (FRPModel m, StateOf m ~ AnimationState, InputOutputOf m ~ Enabled) =>
                 FRP e m (EnergyColor, Point3D, Point3D) ()
lightningBolt = proc (e,p1,p5) ->
    do let radius = 0.01
       p2 <- randomLightningPoint -< (0.25,1,p1,p5)
       p3 <- randomLightningPoint -< (0.25,2,p1,p5)
       p4 <- randomLightningPoint -< (0.25,3,p1,p5)
       lightningBoltSegment -< (e,radius,p1,p2)
       lightningBoltSegment -< (e,radius,p2,p3)
       lightningBoltSegment -< (e,radius,p3,p4)
       lightningBoltSegment -< (e,radius,p4,p5)

randomLightningPoint :: (FRPModel m, StateOf m ~ AnimationState) => FRP e m (RSdouble,Integer,Point3D,Point3D) Point3D
randomLightningPoint = proc (interval,u,a,b) ->
    do let p_base  = lerp (max 0 $ min 1 $ fromInteger u*interval) (a,b)
       let scale_factor = interval * (distanceBetween a b)
       x <- randomA -< (-1,1)
       y <- randomA -< (-1,1)
       z <- randomA -< (-1,1)
       returnA -< translate (vectorScaleTo scale_factor $ Vector3D x y z) p_base

lightningBoltSegment :: (FRPModel m, StateOf m ~ AnimationState, InputOutputOf m ~ Enabled) =>
                        FRP e m (EnergyColor,RSdouble,Point3D,Point3D) ()
lightningBoltSegment = proc (e,radius,a,b) ->
    do transformA libraryA -< (affineOf $ translate (vectorToFrom b origin_point_3d) .
                                          rotateToFrom (vectorToFrom a b) (Vector3D 0 1 0) .
                                          scale (Vector3D radius (distanceBetween a b) radius),
                               (scene_layer_local,EnergyThing EnergyCylinder e))



planetaryAnchorAvatar :: (FRPModel m) => BuildingAvatar e m
planetaryAnchorAvatar = genericBuildingAvatar $ translate (Vector3D 0 0.0 1.0) $ proc () ->
    do libraryA -< (scene_layer_local,PlanetaryAnchorCore)
       planetaryAnchorFlange (1.1^1) (fromDegrees 25) (fromDegrees 30) 10.0 -< ()
       planetaryAnchorFlange (1.1^2) (fromDegrees 50) (fromDegrees 60) 9.0 -< ()
       planetaryAnchorFlange (1.1^3) (fromDegrees 75) (fromDegrees 90) 7.0 -< ()
       planetaryAnchorFlange (1.1^4) (fromDegrees 100) (fromDegrees 120) 4.0 -< ()
       planetaryAnchorFlange (1.1^5) (fromDegrees 125) (fromDegrees 150) 1.0 -< ()
       accumulateSceneA -< (scene_layer_local,
                            lightSource $ PointLight (Point3D 0 0.0 1.0)
                                                     (measure (Point3D 0 0.0 1.0) (Point3D 0 0 0))
                                                     white
                                                     violet)

planetaryAnchorFlange :: (FRPModel m, StateOf m ~ AnimationState, InputOutputOf m ~ Enabled) =>
                         RSdouble -> Angle -> Angle -> RSdouble -> FRP e m () ()
planetaryAnchorFlange s rx rz x = scale' s $ proc () ->
    do rotateA (Vector3D 0 1 0) (perSecond $ fromDegrees $ x*3.0) (rotate (Vector3D 0 0 1) rz $
           rotateA (Vector3D 0 0 1) (perSecond $ fromDegrees $ x*7.0) (rotate (Vector3D 1 0 0) rx $
               rotateA (Vector3D 1 0 0) (perSecond $ fromDegrees $ x*2.0) libraryA)) -<
                   (scene_layer_local,PlanetaryAnchorFlange)


