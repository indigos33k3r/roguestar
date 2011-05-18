module Models.Materials
    (
     -- | Materials by Faction
     treaty_metal,
     treaty_glow,
     treaty_energy_field,
     alliance_metal,
     concordance_metal,
     concordance_dark_glass,
     concordance_bright_glass,
     cyborg_skin,
     cyborg_metal,
     cyborg_glow,
     -- | Material by Species
     caduceator_skin,
     encephalon_skin,
     reptilian_skin,
     reptilian_pigment,
     reptilian_specular,
     hellion_skin,
     -- | Material by Energy Type
     energyColor,
     energyMaterial)
    where

import RSAGL.Modeling
import RSAGL.Math.AbstractVector
import RSAGL.Color
import RSAGL.Color.RSAGLColors
import Models.LibraryData

{---------------------------------------------------------
 - Materials by Faction
 - -------------------------------------------------------}

-- Treaty Organization Materials (cyan-teal solid colors, yellow energy colors)

treaty_metal :: MaterialM ()
treaty_metal = material $
    do pigment $ pure turquoise
       specular 1 $ pure $ teal

treaty_glow :: MaterialM ()
treaty_glow = material $
    do pigment $ pure black
       emissive $ pure mustard

treaty_energy_field :: MaterialM ()
treaty_energy_field = material $
    do emissive $ pure mustard

-- Alliance Materials  (yellow-gold solid colors, orange energy colors)

alliance_metal :: MaterialM ()
alliance_metal =
    do pigment $ pure $ scalarMultiply 0.6 yellow
       specular 7 $ pure yellow

-- Concordance Materials  (violet solid colors, blue energy colors)

concordance_metal :: Modeling
concordance_metal = material $
    do pigment $ pure mauve
       specular 4 $ pure lilac

concordance_dark_glass :: Modeling
concordance_dark_glass = material $
    do pigment $ pure black
       specular 8 $ pure royal_blue

concordance_bright_glass :: Modeling
concordance_bright_glass = material $
    do pigment $ pure black
       emissive $ pure royal_blue
       specular 8 $ pure blue

-- Pirates  (green solid colors, red energy colors)

-- Utopiate (red solid colors, cyan-teal energy colors)

-- Whispers (black solid colors, white energy colors)

-- Monster (orange solid colors, violet energy colors)

-- Cyborg Materials  (white solid colors, green energy colors)
cyborg_skin :: MaterialM ()
cyborg_skin =
    do pigment $ pattern (cloudy 435 0.2) [(0.0,pure mauve),(1.0,pure grey)]

cyborg_metal :: MaterialM ()
cyborg_metal = metallic $ pure beige

cyborg_glow :: MaterialM ()
cyborg_glow =
    do pigment $ pure blackbody
       emissive $ pure $ scalarMultiply 1.0 pale_green

{-------------------------------------------------------
 - Materials by Species
 - -----------------------------------------------------}

-- Caduceator Skins

caduceator_skin :: MaterialM ()
caduceator_skin = pigment $ pattern (cloudy 75 0.01) [(0.0,pure red),(0.5,pure orange),(1.0,pure black)]

-- Reptilian Skins

reptilian_pigment :: ColorFunction RGB
reptilian_pigment = pattern (cloudy 75 0.1) [(0.0,pure lavender),(1.0,pure periwinkle)]

reptilian_specular :: ColorFunction RGB
reptilian_specular = pattern (cloudy 75 0.1) [(0.0,pure red),(1.0,pure mustard)]

reptilian_skin :: MaterialM ()
reptilian_skin =
    do pigment $ reptilian_pigment
       specular 5.0 $ reptilian_specular

-- Hellion Skin

hellion_skin :: MaterialM ()
hellion_skin =
    do pigment $ pattern (cloudy 75 0.1) [(0.0,pure sea_green),(1.0,pure lime)]
       specular 5.0 $ scalarMultiply (1/5) pure white

-- Encephalon Skins

encephalon_skin :: MaterialM ()
encephalon_skin = pigment $ pattern (cloudy 32 0.1) [(0.0,pure mauve),(1.0,pure salmon)]

{--------------------------------------------------------
 - Material by Energy Type
 - ------------------------------------------------------}

--  Blue:   0.1 0.3 0.9
--  Yellow: 0.3 0.3 0.0
--  Red:    0.6 0.0 0.0
--  Green:  0.0 0.4 0.1
--  Sum to: 1.0 1.0 1.0
energyColor :: EnergyColor -> RGB
energyColor Blue = RGB 0.1 0.3 0.9
energyColor Yellow = RGB 0.3 0.3 0.0
energyColor Red = RGB 0.6 0.0 0.0
energyColor Green = RGB 0.0 0.4 0.1

energyMaterial :: EnergyColor -> Modeling
energyMaterial c = material $
    do pigment $ pure $ scalarMultiply 0.33 $ energyColor c
       specular 1.0 $ pure $ scalarMultiply 0.33 $ energyColor c
       emissive $ pure $ scalarMultiply 0.33 $ energyColor c

