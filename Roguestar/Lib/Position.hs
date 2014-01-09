--Data
module Roguestar.Lib.Position
    (Position(..),
     MultiPosition(..),
     multiPosition,
     ToPosition(..),
     ToMultiPosition(..),
     OffsetPosition(..),
     distanceBetweenSquared,
     distanceBetweenChessboard,
     positionPairs)
    where

import qualified Data.Set as Set

-- | Position of an object in \"chessboard space\".
newtype Position = Position { fromPosition :: (Integer,Integer) }
    deriving (Eq,Ord,Read,Show)

-- | For objects, such as buildings, that occupy multiple positions.
newtype MultiPosition = MultiPosition { fromMultiPosition :: Set.Set Position }

instance Eq MultiPosition where
    (==) (MultiPosition as) (MultiPosition bs) = as == bs

instance Ord MultiPosition where
    compare (MultiPosition as) (MultiPosition bs) = as `compare` bs

class (ToMultiPosition p) => ToPosition p where
    toPosition :: p -> Position

instance ToPosition Position where
    toPosition = id

class ToMultiPosition p where
    toMultiPosition :: p -> MultiPosition

class (ToMultiPosition p) => OffsetPosition p where
    offsetPosition :: (Integer,Integer) -> p -> p

instance ToMultiPosition Position where
    toMultiPosition p = MultiPosition $ Set.singleton p

instance OffsetPosition Position where
    offsetPosition (x,y) (Position (u,v)) = Position (x+u,y+v)

instance ToMultiPosition MultiPosition where
    toMultiPosition = id

instance OffsetPosition MultiPosition where
    offsetPosition xy (MultiPosition ps) = MultiPosition $ Set.map (offsetPosition xy) ps

instance ToMultiPosition a => ToMultiPosition [a] where
    toMultiPosition = MultiPosition . Set.unions . map (fromMultiPosition . toMultiPosition)

instance ToMultiPosition a => ToMultiPosition (Set.Set a) where
    toMultiPosition =  toMultiPosition . Set.toList

-- | Construct a 'MultiPosition' from a base position and a list of offsets.
-- The base position always counts as part of the MultiPosition.
multiPosition :: Position -> [(Integer,Integer)] -> MultiPosition
multiPosition (Position xy) xys = MultiPosition $ Set.fromList $ Position xy : map (offsetPosition xy . Position) xys

-- | Pythagorean distance, squared.
-- For multi-positions, measures the minimal distance.
distanceBetweenSquared :: (ToMultiPosition a,ToMultiPosition b) => a -> b -> Integer
distanceBetweenSquared as bs = minimum $
    do (Position (x,y), Position (u,v)) <- positionPairs as bs
       return $ (x - u)^2 + (y - v)^2

-- | Number of squares you would have to move (as a queen on a chessboard) to arrive from the first position to the second.
-- For multi-positions, measures the minimal distance.
distanceBetweenChessboard :: (ToMultiPosition a,ToMultiPosition b) => a -> b -> Integer
distanceBetweenChessboard as bs = minimum $
    do (Position (x,y), Position (u,v)) <- positionPairs as bs
       return $ max (abs $ u - x) (abs $ v - y)

-- | List all pairs of positions between two MutiPositions.
positionPairs :: (ToMultiPosition a,ToMultiPosition b) => a -> b -> [(Position,Position)]
positionPairs as bs =
    do a <- Set.toList $ fromMultiPosition $ toMultiPosition as
       b <- Set.toList $ fromMultiPosition $ toMultiPosition bs
       return (a,b)
