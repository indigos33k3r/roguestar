--Data
module Roguestar.Lib.PersistantData
    (CharacterClass(..),
     PowerUpData(..),
     PlanetRegion(..),
     Building(..),
     BuildingBehavior(..),
     BuildingShape(..),
     BuildingSignal(..),
     BuildingPrototype(..))
    where

{----- CHARACTER -----}

data CharacterClass = StarChild
                    deriving (Eq,Enum,Bounded,Read,Show,Ord)

{----- POWER UPS -----}

-- |
-- Cause a character to advance in level or to gain a specific CharacterClass.
data PowerUpData =
    -- Award a character points.  If the character gain enough points to advance in character class,
    -- then do this, otherwise, he just accumulates the points.
    AwardCharacter Integer
    -- Apply a specific CharacterClass to a character.  If he already has this CharacterClass,
    -- then we back off and give him the points instead.
  | ForceCharacter CharacterClass
      deriving (Eq,Read,Show)

{----- PLANETS -----}

data PlanetRegion = NonAlignedRegion | CyborgRegion
    deriving (Eq,Ord,Read,Show)

{----- BUILDINGS -----}

data Building = Building { building_behavior :: BuildingBehavior,
                           building_signal :: Maybe BuildingSignal }
        deriving (Read,Show)

data BuildingBehavior = PowerUp PowerUpData | TwoWayStargate PlanetRegion | OneWayStargate PlanetRegion
        deriving (Eq,Read,Show)

data BuildingShape = Monolith | Anchor | Portal | CyberGate
        deriving (Eq,Ord,Read,Show)

data BuildingSignal = Magnetic
        deriving (Eq,Read,Show)

data BuildingPrototype = BuildingPrototype {    -- TODO: does this data structure really need to be persistant?
     buildingproto_behavior :: BuildingBehavior,
     buildingproto_shape :: BuildingShape,
     buildingproto_signal :: Maybe BuildingSignal }
          deriving (Eq,Read,Show)

