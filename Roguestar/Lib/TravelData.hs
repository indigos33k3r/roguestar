--Data
module Roguestar.Lib.TravelData
    (ClimbDirection(..)) where

data ClimbDirection = ClimbUp | ClimbDown
    deriving (Read,Show,Eq)
