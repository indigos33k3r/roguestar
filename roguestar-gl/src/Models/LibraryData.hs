module Models.LibraryData
    (LibraryModel(..),
     SimpleModel(..),
     FactionedModel(..),
     EnergyColor(..),
     EnergyThing(..),
     LibraryModelSource(..))
    where

import Models.Sky
import Models.FactionData
import qualified Data.ByteString.Char8 as B

data EnergyColor = Blue | Yellow | Red | Green
         deriving (Eq,Ord,Show,Enum,Bounded)

data SimpleModel =
    LeafyBlob
  | TreeBranch
  | QuestionMark
    -- Vortex Glows/Puffs
  | AscendantGlow
  | DustPuff
    -- Tools
  | PhasePistol
  | Phaser
  | PhaseRifle
  | GasSphere
  | MetalSphere
  | ChromaliteSphere
    -- Space Ship Parts
  | CyborgType4Dome
  | CyborgType4Base
  | CyborgType4HyperspaceDisc
  | CyborgType4HyperspaceRotor
  | CyborgType4HyperspaceStabilizer
    -- Buildings
  | Monolith
  | PlanetaryAnchorCore
  | PlanetaryAnchorFlange
  | Portal
  | Cybergate
  | Cyberpylon
      deriving (Eq,Ord,Show,Enum,Bounded)

data EnergyThing =
    EnergySword
  | EnergyCylinder
      deriving (Eq,Ord,Show,Enum,Bounded)

data FactionedModel =
    -- Creature Bodies
    Encephalon
  | Recreant
  | Androsynth
  | Caduceator
  | Reptilian
  | Hellion
    -- Arms and Legs
  | MachineArmLower
  | MachineArmUpper
  | CaduceatorArmLower
  | CaduceatorArmUpper
  | ReptilianLegUpper
  | ReptilianLegLower
  | ReptilianArmUpper
  | ReptilianArmLower
  | HellionAppendage
  | ThinLimb
    -- Other bodyparts
  | HellionEye
    deriving(Eq, Ord, Show)

data LibraryModel =
    -- Terrain
    TerrainTile B.ByteString
    -- Astronomical Phenomena
  | SkySphere SkyInfo
  | SunDisc SunInfo
    -- The Null Model
  | NullModel
    -- SimpleModels (zero-parameter models)
  | SimpleModel SimpleModel
    -- Energy things
  | EnergyThing EnergyThing EnergyColor
    -- Factioned Models
  | FactionedModel Faction FactionedModel
      deriving (Eq,Ord,Show)

-- | Things that are also LibraryModels.
class LibraryModelSource a where
    toLibraryModel :: a -> LibraryModel

instance LibraryModelSource LibraryModel where
    toLibraryModel = id

instance LibraryModelSource SimpleModel where
    toLibraryModel = SimpleModel

