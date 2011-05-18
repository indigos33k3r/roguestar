{-# LANGUAGE Arrows, OverloadedStrings, TypeFamilies #-}

module AnimationCreatures
    (creatureAvatar)
    where

import RSAGL.FRP
import RSAGL.Math
import RSAGL.Animation
import RSAGL.Color.RSAGLColors
import Animation
import Control.Arrow
import Models.LibraryData
import VisibleObject
import Limbs
import Scene
import AnimationExtras
import AnimationVortex
import CreatureData
import qualified Data.ByteString.Char8 as B

-- | Avatar for any creature that automatically switches to the appropriate species-specific avatar thread.
creatureAvatar :: (FRPModel m) => CreatureAvatar e m
creatureAvatar = proc () ->
    do objectTypeGuard (== "creature") -< ()
       m_species <- objectDetailsLookup ThisObject "species" -< ()
       switchContinue -< (fmap switchTo m_species,())
       returnA -< Nothing
  where switchTo "encephalon" = encephalonAvatar
        switchTo "recreant" = recreantAvatar
        switchTo "androsynth" = androsynthAvatar
        switchTo "ascendant" = ascendantAvatar
        switchTo "caduceator" = caduceatorAvatar
        switchTo "reptilian" = reptilianAvatar
        switchTo "hellion" = hellionAvatar
        switchTo "dustvortex" = dustVortexAvatar
        switchTo _ = questionMarkAvatar

encephalonAvatar :: (FRPModel m) => CreatureAvatar e m
encephalonAvatar = genericCreatureAvatar normal $ proc () ->
    do faction <- objectFaction ThisObject -< ()
       libraryA -< (scene_layer_local,FactionedModel faction Encephalon)
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms (Vector3D 0.66 0 0.66) (Point3D 0.145 0 0.145) 0.33 (Point3D 0.35 0.133 0.0666) -< (FactionedModel faction MachineArmUpper,FactionedModel faction MachineArmLower)
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

recreantAvatar :: (FRPModel m) => CreatureAvatar e m
recreantAvatar = genericCreatureAvatar normal $ floatBobbing 0.25 0.4 $ proc () ->
    do faction <- objectFaction ThisObject -< ()
       libraryA -< (scene_layer_local,FactionedModel faction Recreant)
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms (Vector3D 0 0 (-1.0)) (Point3D 0.3 0 0.075) 0.5 (Point3D 0.5 0.2 0.075) -< (FactionedModel faction MachineArmUpper,FactionedModel faction MachineArmLower)
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

androsynthAvatar :: (FRPModel m) => CreatureAvatar e m
androsynthAvatar = genericCreatureAvatar normal $ proc () ->
    do faction <- objectFaction ThisObject -< ()
       libraryA -< (scene_layer_local,FactionedModel faction Androsynth)
       bothLegs Upright (Vector3D 0 1 0) (Point3D 0.07 0 0.5) 0.54 (Point3D 0.07 0 0) -< (FactionedModel faction ThinLimb, FactionedModel faction ThinLimb)
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms (Vector3D (1.0) (-1.0) (-1.0)) (Point3D 0.05 0.0 0.65) 0.45 (Point3D 0.15 0.1 0.34) -< (FactionedModel faction ThinLimb,FactionedModel faction ThinLimb)
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

ascendantAvatar :: (FRPModel m) => CreatureAvatar e m
ascendantAvatar = particleAvatar vortex 12 (SimpleModel AscendantGlow) $ Just light_blue

dust_vortex :: Vortex
dust_vortex = vortex {
    vortex_rotation = \x -> if x > 0.001 then recip x else 0,
    vortex_binding = 0,
    vortex_containment = 0.0,
    vortex_base_angle = fromDegrees 45,
    vortex_repulsion = 0.4,
    vortex_height = -0.1,
    vortex_gravity = 15,
    vortex_base_force = 120 }

dustVortexAvatar :: (FRPModel m) => CreatureAvatar e m
dustVortexAvatar = particleAvatar dust_vortex 12 (SimpleModel DustPuff) Nothing

caduceatorAvatar :: (FRPModel m) => CreatureAvatar e m
caduceatorAvatar = genericCreatureAvatar normal $ proc () ->
    do faction <- objectFaction ThisObject -< ()
       libraryA -< (scene_layer_local,FactionedModel faction Caduceator)
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms (Vector3D 1.0 1.0 (-1.0)) (Point3D 0.1 0.257 0.15) 0.34 (Point3D 0.02 0.4 0.17) -< (FactionedModel faction CaduceatorArmUpper, FactionedModel faction CaduceatorArmLower)
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

reptilianAvatar :: (FRPModel m) => CreatureAvatar e m
reptilianAvatar = genericCreatureAvatar normal $ proc () ->
    do faction <- objectFaction ThisObject -< ()
       libraryA -< (scene_layer_local,FactionedModel faction Reptilian)
       bothLegs Upright (Vector3D 0 1 0) (Point3D (0.05) (-0.1) 0.25) 0.29 (Point3D 0.07 0 0) -< (FactionedModel faction ReptilianLegUpper,FactionedModel faction ReptilianLegLower)
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms (Vector3D 1.0 1.0 0.0) (Point3D (0.05) (-0.1) 0.35) 0.25 (Point3D 0.07 0.12 0.25) -< (FactionedModel faction ReptilianArmUpper, FactionedModel faction ReptilianArmLower)
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }

hellionAvatar :: (FRPModel m) => CreatureAvatar e m
hellionAvatar = genericCreatureAvatar normal $ proc () ->
    do faction <- objectFaction ThisObject -< ()
       libraryA -< (scene_layer_local,FactionedModel faction Hellion)
       bothEyeStalks (Vector3D (0.1) (-1) 0)
                     (Point3D 0.06 0 0.55)
                     1.2
                     (Point3D 0.2 0.05 0.8) -< (FactionedModel faction HellionAppendage,
                                                FactionedModel faction HellionAppendage,
                                                FactionedModel faction HellionEye)
       bothLegs Upright (Vector3D 0.5 (-1) 0) (Point3D 0.05 0 0.55) 0.8 (Point3D 0.05 0 0) -< (FactionedModel faction HellionAppendage,FactionedModel faction HellionAppendage)
       wield_point <- exportCoordinateSystem <<< arr (joint_arm_hand . snd) <<<
           bothArms (Vector3D 1.0 (-0.5) 0) (Point3D 0.1 0 0.6) 0.4 (Point3D 0.3 0.3 0.25) -< (FactionedModel faction HellionAppendage,FactionedModel faction HellionAppendage)
       returnA -< CreatureThreadOutput {
           cto_wield_point = wield_point }


