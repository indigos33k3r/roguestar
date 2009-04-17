{-# LANGUAGE Arrows #-}

module AnimationTools
    (toolAvatar)
    where

import RSAGL.Math
import Scene
import Animation
import RSAGL.Animation
import RSAGL.FRP
import Control.Arrow
import VisibleObject
import Models.LibraryData
import Control.Applicative

-- | Avatar for any tool that automatically switched to the correct tool-specific thread.
toolAvatar :: RSAnimA (Maybe Integer) ToolThreadInput () ToolThreadInput ()
toolAvatar = proc tti ->
    do objectTypeGuard (== "tool") -< ()
       m_tool <- objectDetailsLookup "tool" -< ()
       m_tool_type <- objectDetailsLookup "tool-type" -< ()
       switchContinue -< (fmap switchTo $ (,) <$> m_tool_type <*> m_tool,tti)
       returnA -< ()
  where switchTo (_,"phase_pistol") = phaseWeaponAvatar PhasePistol
        switchTo (_,"phaser") = phaseWeaponAvatar Phaser
        switchTo (_,"phase_rifle") = phaseWeaponAvatar PhaseRifle
        switchTo (_,"kinetic_fleuret") = energySwordAvatar Yellow 2
        switchTo (_,"kinetic_sabre") = energySwordAvatar Yellow 4
        switchTo ("sphere-gas",gas) = gasSphereAvatar gas
        switchTo ("sphere-material",material) = materialSphereAvatar material
        switchTo ("sphere-chromalite",chromalite) = chromaliteSphereAvatar chromalite
        switchTo _ = questionMarkAvatar >>> arr (const ())

simpleToolAvatar :: LibraryModel -> RSAnimA (Maybe Integer) ToolThreadInput () ToolThreadInput ()
simpleToolAvatar phase_weapon_model = proc tti ->
    do visibleObjectHeader -< ()
       m_orientation <- wieldableObjectIdealOrientation -< tti
       whenJust (transformA libraryA) -< fmap (\o -> (o,(scene_layer_local,phase_weapon_model))) m_orientation
       returnA -< ()

phaseWeaponAvatar :: LibraryModel -> RSAnimA (Maybe Integer) ToolThreadInput () ToolThreadInput ()
phaseWeaponAvatar = simpleToolAvatar

energySwordAvatar :: EnergyColor -> Integer -> RSAnimA (Maybe Integer) ToolThreadInput () ToolThreadInput ()
energySwordAvatar energy_color sword_size = proc tti ->
    do visibleObjectHeader -< ()
       m_orientation <- wieldableObjectIdealOrientation -< tti
       is_being_wielded <- isBeingWielded -< ()
       whenJust (transformA displayA) -< fmap (\o -> (o,is_being_wielded)) m_orientation
       returnA -< ()
  where displayA :: RSAnimA1 i o Bool ()
        displayA = scale' (1/75) $ proc is_being_wielded ->
            do blade_length <- approachFrom 1 (perSecond 65) 0 -< if is_being_wielded then 10 * realToFrac sword_size else 0
               libraryA -< (scene_layer_local,EnergySword energy_color sword_size)
               transformA libraryA -< (Affine $ translate (Vector3D 0 2.9 0) . scale (Vector3D 1 blade_length 1),(scene_layer_local,EnergyCylinder energy_color))

gasSphereAvatar :: String -> RSAnimA (Maybe Integer) ToolThreadInput () ToolThreadInput ()
gasSphereAvatar = simpleToolAvatar . gasToModel
    where gasToModel :: String -> LibraryModel
          gasToModel = const GasSphere

materialSphereAvatar :: String -> RSAnimA (Maybe Integer) ToolThreadInput () ToolThreadInput ()
materialSphereAvatar = simpleToolAvatar . materialToModel
    where materialToModel :: String -> LibraryModel
          materialToModel = const MetalSphere

chromaliteSphereAvatar :: String -> RSAnimA (Maybe Integer) ToolThreadInput () ToolThreadInput ()
chromaliteSphereAvatar = simpleToolAvatar . const ChromaliteSphere
          
