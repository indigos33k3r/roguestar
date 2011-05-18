{-# LANGUAGE Arrows #-}

module CreatureData
    (CreatureAvatarSwitch,
     CreatureAvatar,
     CreatureAvatarConfiguration,
     normal,
     nonrotating,
     genericCreatureAvatar)
    where

import RSAGL.FRP
import RSAGL.Scene
import RSAGL.Math
import VisibleObject
import Data.Maybe
import Control.Arrow

type CreatureAvatarSwitch m = AvatarSwitch () (Maybe CreatureThreadOutput) m
type CreatureAvatar e m = FRP e (AvatarSwitch () (Maybe CreatureThreadOutput) m) () (Maybe CreatureThreadOutput)

data CreatureAvatarConfiguration = CreatureAvatarConfiguration {
    should_rotate :: Bool }

normal :: CreatureAvatarConfiguration
normal = CreatureAvatarConfiguration {
    should_rotate = True }

nonrotating :: CreatureAvatarConfiguration
nonrotating = CreatureAvatarConfiguration {
    should_rotate = False }

genericCreatureAvatar :: (FRPModel m) =>
                         CreatureAvatarConfiguration ->
                         FRP e (CreatureAvatarSwitch m) () CreatureThreadOutput -> CreatureAvatar e m
genericCreatureAvatar config creatureA = proc () ->
    do visibleObjectHeader -< ()
       m_position_info <- objectIdealPosition ThisObject -< ()
       m_orientation <- objectIdealOrientation ThisObject -< ()
       let m_coordinate_system = if (should_rotate config) then m_orientation else
               fmap (\position -> translate (vectorToFrom position origin_point_3d) root_coordinate_system) m_position_info
       switchTerminate -< if isNothing m_orientation then (Just $ genericCreatureAvatar config creatureA,Nothing) else (Nothing,Nothing)
       arr Just <<< transformA creatureA -< (fromMaybe (error "genericCreatureAvatar: fromMaybe") m_coordinate_system,())

