
module Models.FactionData
    (Faction(..))
    where

data Faction =
    Player
  | Nonaligned
  | Monsters
  | Cyborg
  | Gray
    deriving (Eq, Ord, Show)
