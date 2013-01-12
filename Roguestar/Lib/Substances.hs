{-# LANGUAGE OverloadedStrings #-}
--Data
module Roguestar.Lib.Substances
    (Gas(..),
     Material(..),
     Chromalite(..),
     Solid(..),
     materialValue,
     chromaliteValue,
     gasValue,
     Substance(..),
     SubstanceType(toSubstance),
     coerceSubstance,
     isGas,
     isMaterial,
     isChromalite,
     substances,
     prettySubstance,
     substanceValue)
    where

import Data.Maybe
import qualified Data.Text as T

data Substance =
    GasSubstance Gas
  | MaterialSubstance Material
  | ChromaliteSubstance Chromalite
             deriving (Read,Show,Eq,Ord)

substances :: [Substance]
substances = map GasSubstance [minBound..maxBound] ++
             map MaterialSubstance [minBound..maxBound] ++
             map ChromaliteSubstance [minBound..maxBound]

prettySubstance :: Substance -> T.Text
prettySubstance (GasSubstance x) = T.pack $ show x
prettySubstance (MaterialSubstance x) = T.pack $ show x
prettySubstance (ChromaliteSubstance x) = T.pack $ show x

data Solid = MaterialSolid Material
           | ChromaliteSolid Chromalite
           deriving (Read,Show,Eq,Ord)

data Gas =
    Water
  | Hydrogen
  | Helium
  | Oxygen
  | Nitrogen
  | Flourine
  | Neon
  | Argon
  | Krypton
  | Xenon
  | Radon
  | Methane
  | Ammonia
  | Iodine
  | Chlorine deriving (Eq,Enum,Ord,Show,Read,Bounded)

data Material =
    Aluminum
  | Titanium
  | Palladium
  | Molybdenum
  | Lead
  | Copper
  | Iron
  | Cobalt
  | Zirconium
  | Gold
  | Silver
  | Platinum
  | Zinc
  | Uranium
  | Plutonium
  | Thorium
  | Diamond
  | Carbon
  | Wood
  | Plastic
  | Silicon
  | Nickel
        deriving (Eq,Enum,Ord,Show,Read,Bounded)

data Chromalite =
    Rutilium     -- red Chromalite
  | Crudnium     -- green Chromalite
  | Pteulanium   -- blue Chromalite
  | Caerulite    -- azure Chromalite
  | Ionidium     -- violet Chromalite
  | Aurite       -- yellow Chromalite
  | Argentate    -- silver Chromalite
  | Trabanate    -- brown Chromalite
  | Arumate      -- gold Chromalite
  | Candonium    -- white Chromalite
  | Canitium     -- gray Chromalite
  | Infuscanoid  -- black Chromalite
  | Endurium     -- blue/shadowy Chromalite
  | Malignite    -- yellow/shadowy Chromalite
  | Diabolite    -- radiant white Chromalite
  | Bectonite    -- radiant black Chromalite
     deriving (Eq,Enum,Ord,Show,Read,Bounded)

gasValue :: Gas -> Integer
gasValue Water = 2
gasValue Hydrogen = 4
gasValue Helium = 6
gasValue Nitrogen = 7
gasValue Oxygen = 10
gasValue Flourine = 12
gasValue Neon = 20
gasValue Ammonia = 21
gasValue Methane = 24
gasValue Chlorine = 30
gasValue Argon = 40
gasValue Krypton = 42
gasValue Xenon = 60
gasValue Radon = 70
gasValue Iodine = 100

materialValue :: Material -> Integer
materialValue Wood = 1
materialValue Plastic = 3
materialValue Lead = 5
materialValue Carbon = 8
materialValue Silicon = 9
materialValue Iron = 11
materialValue Zinc = 13
materialValue Palladium = 15
materialValue Nickel = 17
materialValue Aluminum = 19
materialValue Molybdenum = 22
materialValue Copper = 23
materialValue Silver = 25
materialValue Cobalt = 28
materialValue Titanium = 31
materialValue Gold = 39
materialValue Zirconium = 45
materialValue Platinum = 48
materialValue Thorium = 50
materialValue Uranium = 80
materialValue Plutonium = 90
materialValue Diamond = 100

chromaliteValue :: Chromalite -> Integer
chromaliteValue Rutilium = 14
chromaliteValue Crudnium = 16
chromaliteValue Pteulanium = 18
chromaliteValue Caerulite = 26
chromaliteValue Ionidium = 29
chromaliteValue Aurite = 32
chromaliteValue Argentate = 36
chromaliteValue Trabanate = 37
chromaliteValue Arumate = 46
chromaliteValue Candonium = 49
chromaliteValue Canitium = 55
chromaliteValue Infuscanoid = 65
chromaliteValue Endurium = 75
chromaliteValue Malignite = 85
chromaliteValue Diabolite = 95
chromaliteValue Bectonite = 100

class SubstanceType a where
    toSubstance :: a -> Substance
    fromSubstance :: Substance -> Maybe a

coerceSubstance :: (SubstanceType a,SubstanceType b) => a -> Maybe b
coerceSubstance = fromSubstance . toSubstance

isGas :: (SubstanceType a) => a -> Bool
isGas = isJust . (`asTypeOf` (undefined :: Maybe Gas)) . coerceSubstance

isMaterial :: (SubstanceType a) => a -> Bool
isMaterial = isJust . (`asTypeOf` (undefined :: Maybe Material)) . coerceSubstance

isChromalite :: (SubstanceType a) => a -> Bool
isChromalite = isJust . (`asTypeOf` (undefined :: Maybe Chromalite)) . coerceSubstance

substanceValue :: (SubstanceType a) => a -> Integer
substanceValue a = case toSubstance a of
    GasSubstance x -> gasValue x + 10
    MaterialSubstance x -> materialValue x * 10
    ChromaliteSubstance x -> 1000 + chromaliteValue x * 100

instance SubstanceType Gas where
    toSubstance = GasSubstance
    fromSubstance (GasSubstance x) = Just x
    fromSubstance _ = Nothing

instance SubstanceType Material where
    toSubstance x = MaterialSubstance x
    fromSubstance (MaterialSubstance x) = Just x
    fromSubstance _ = Nothing

instance SubstanceType Chromalite where
    toSubstance x = ChromaliteSubstance x
    fromSubstance (ChromaliteSubstance x) = Just x
    fromSubstance _ = Nothing

instance SubstanceType Substance where
    toSubstance x = x
    fromSubstance = Just

instance SubstanceType Solid where
    toSubstance (MaterialSolid x) = toSubstance x
    toSubstance (ChromaliteSolid x) = toSubstance x
    fromSubstance (MaterialSubstance x) = Just $ MaterialSolid x
    fromSubstance (ChromaliteSubstance x) = Just $ ChromaliteSolid x
    fromSubstance _ = Nothing

