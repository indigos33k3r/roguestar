module Roguestar.Lib.Core.Entities
    (getAncestors)
    where

import Roguestar.Lib.Data.LocationData
import Roguestar.Lib.DB

getAncestors :: Reference a -> DB_BaseType -> [Location]
getAncestors reference _ | reference =:= the_universe = []
getAncestors reference db = location : getAncestors reference' db
    where reference' = parentReference location
          location = whereIs reference db
