{-# LANGUAGE TypeFamilies #-}

module Roguestar.Lib.DetailedTravel
    (Roguestar.Lib.DetailedTravel.whereIs,
     Roguestar.Lib.DetailedTravel.getContents)
    where

import Prelude hiding (getContents)
import Roguestar.Lib.DB as DB
import Roguestar.Lib.DetailedLocation
import Control.Monad
import Data.Maybe
import Roguestar.Lib.Reference

whereIs :: (DBReadable db,ReferenceType a) => Reference a -> db (DetailedLocation (Child a))
whereIs = liftM (fromMaybe (error "DetailedTravel.whereIs: Reference is not a child of it's own location.") . fromLocation) . DB.whereIs

getContents :: (DBReadable db,ReferenceType a) => Reference a -> db [DetailedLocation (Parent a)]
getContents = liftM mapLocations . DB.getContents

