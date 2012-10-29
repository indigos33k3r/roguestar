--Data -- this module is dubious, may deprecate
module Roguestar.Lib.TimeCoordinate
    (TimeCoordinate,
     advanceTime,
     zero_time)
    where

data TimeCoordinate = TimeCoordinate Rational
    deriving (Eq,Ord,Read,Show)

advanceTime :: Rational -> TimeCoordinate -> TimeCoordinate
advanceTime x (TimeCoordinate t) = TimeCoordinate (t + x)

zero_time :: TimeCoordinate
zero_time = TimeCoordinate 0
