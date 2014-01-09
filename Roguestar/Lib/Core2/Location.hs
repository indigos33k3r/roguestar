module Roguestar.Lib.Core2.Location
    (standing)
    where

import Roguestar.Lib.Data.FacingData
import Roguestar.Lib.Data.LocationData
import Roguestar.Lib.Data.ReferenceTypes
import Roguestar.Lib.Graph.Classes

standing :: (HasSquare a) => Facing -> a -> Standing
standing face x = Standing (toReference $ plane $ square x)
                           (position $ square x)
                           face
