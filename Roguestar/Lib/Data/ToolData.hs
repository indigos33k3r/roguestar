{-# LANGUAGE OverloadedStrings #-}

--Data
module Roguestar.Lib.Data.ToolData
    (Tool(..),
     toolName,
     fromSphere,
     sphere,
     Device(..),
     DeviceKind(..),
     DeviceFunction(..),
     deviceName,
     deviceValue,
     improvised,
     phase_pistol,
     phaser,
     phase_rifle,
     kinetic_fleuret,
     kinetic_sabre)
    where

import Roguestar.Lib.Data.Substances
import qualified Data.Text as T

data Tool = DeviceTool DeviceFunction Device
          | Sphere Substance
    deriving (Read,Show,Eq)

toolName :: Tool -> T.Text
toolName (DeviceTool _ d) = deviceName d
toolName (Sphere s) = prettySubstance s

-- | Get the substance type of a material sphere, if it is one.
fromSphere :: Tool -> Maybe Substance
fromSphere (Sphere s) = Just s
fromSphere _ = Nothing

sphere :: (SubstanceType a) => a -> Tool
sphere = Sphere . toSubstance

data DeviceFunction = Gun | Sword
            deriving (Read,Show,Eq)

data DeviceKind =
    Pistol
  | Carbine
  | Rifle
  | Fleuret
  | Sabre
        deriving (Read,Show,Eq)

kindToFunction :: DeviceKind -> (DeviceFunction,Integer)
kindToFunction Pistol = (Gun,1)
kindToFunction Carbine = (Gun,3)
kindToFunction Rifle = (Gun,5)
kindToFunction Fleuret = (Sword,2)
kindToFunction Sabre = (Sword,4)

-- | Any kind of device that is constructed from a power cell, materal, and gas medium,
-- using the various device rules to determine it's power.
data Device = Device {
   device_name :: T.Text,
   device_chromalite :: Chromalite,
   device_material :: Material,
   device_gas :: Gas,
   device_size :: Integer }
     deriving (Eq,Read,Show)

device :: T.Text -> DeviceKind -> Chromalite -> Material -> Gas -> Tool
device s dk c m g = DeviceTool func (Device s c m g size)
    where (func,size) = kindToFunction dk

improvised :: DeviceKind -> Chromalite -> Material -> Gas -> Tool
improvised dk c m g = device ("improvised_" `T.append` T.pack (show dk)) dk c m g

phase_pistol :: Tool
phase_pistol = device "phase_pistol" Pistol Caerulite Zinc Flourine

phaser :: Tool
phaser = device "phaser" Carbine Caerulite Zinc Flourine

phase_rifle :: Tool
phase_rifle = device "phase_rifle" Rifle Caerulite Zinc Flourine

kinetic_fleuret :: Tool
kinetic_fleuret = device "kinetic_fleuret" Fleuret Ionidium Aluminum Nitrogen

kinetic_sabre :: Tool
kinetic_sabre = device "kinetic_sabre" Sabre Ionidium Aluminum Nitrogen

deviceName :: Device -> T.Text
deviceName = device_name

deviceValue :: Device -> Integer
deviceValue d = device_size d * (gasValue $ device_gas d) +
                                (materialValue $ device_material d) +
                                (chromaliteValue $ device_chromalite d)

