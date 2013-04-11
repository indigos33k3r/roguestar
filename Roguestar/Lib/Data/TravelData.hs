--Data
module Roguestar.Lib.Data.TravelData
    (ClimbDirection(..)) where

data ClimbDirection = ClimbUp | ClimbDown
    deriving (Read,Show,Eq)
