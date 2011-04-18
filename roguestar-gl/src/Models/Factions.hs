module Models.Factions
    (skin,
     metal,
     energyField,
     eye,
     iris)
    where

import Models.FactionData
import Models.Materials
import RSAGL.Modeling.Model
import RSAGL.Color.RSAGLColors
import Control.Applicative

skin :: Faction -> MaterialM () -> MaterialM ()
skin Player = id
skin Nonaligned = id
skin Monsters = id
skin Cyborg = const cyborg_skin
skin Gray = const $ pigment (pure blackbody) >> emissive (pure grey)

metal :: Faction -> MaterialM ()
metal Player = alliance_metal
metal Nonaligned = alliance_metal
metal Monsters = alliance_metal
metal Cyborg = cyborg_metal
metal Gray = pigment (pure blackbody) >> emissive (pure grey)

energyField :: Faction -> MaterialM ()
energyField Player = treaty_energy_field
energyField Nonaligned = treaty_energy_field
energyField Monsters = treaty_energy_field
energyField Cyborg = do emissive $ pure red
energyField Gray = emissive (pure grey)

eye :: Faction -> MaterialM ()
eye Player = pigment $ pure black
eye Nonaligned = pigment $ pure black
eye Monsters = pigment $ pure black
eye Cyborg = do pigment $ pure blackbody
                energyField Cyborg
eye Gray = pigment (pure blackbody) >> emissive (pure grey)

iris :: Faction -> MaterialM ()
iris Player = pigment $ pure blue
iris Nonaligned = pigment $ pure light_brown
iris Monsters = pigment $ pure maroon
iris Cyborg = do pigment $ pure white
iris Gray = pigment (pure blackbody) >> emissive (pure grey)

