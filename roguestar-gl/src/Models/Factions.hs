module Models.Factions
    (Faction(..),
     skin,
     metal,
     energyField,
     eye)
    where

import Models.Materials
import RSAGL.Modeling.Model
import RSAGL.Color.RSAGLColors
import Control.Applicative

data Faction =
    Player
  | Nonaligned
  | Monsters
  | Cyborg

skin :: Faction -> MaterialM () -> MaterialM ()
skin Player = id
skin Nonaligned = id
skin Monsters = id
skin Cyborg = const cyborg_skin

metal :: Faction -> MaterialM ()
metal Player = alliance_metal
metal Nonaligned = alliance_metal
metal Monsters = alliance_metal
metal Cyborg = cyborg_metal

energyField :: Faction -> MaterialM ()
energyField Player = treaty_energy_field
energyField Nonaligned = treaty_energy_field
energyField Monsters = treaty_energy_field
energyField Cyborg = do emissive $ pure red

eye :: Faction -> MaterialM ()
eye Player = pigment $ pure black
eye Nonaligned = pigment $ pure black
eye Monsters = pigment $ pure black
eye Cyborg = do pigment $ pure blackbody
                energyField Cyborg

