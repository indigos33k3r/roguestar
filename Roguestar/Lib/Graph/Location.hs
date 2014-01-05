module Roguestar.Lib.Graph.Location
    (standing)
    where

import Roguestar.Lib.Data.FacingData
import Roguestar.Lib.Data.LocationData
import Roguestar.Lib.Graph.Classes

standing :: (HasSquare a) => Facing -> a -> Standing
standing face x = Standing (planeReference $ plane $ square x)
                           (position $ square x)
                           face


