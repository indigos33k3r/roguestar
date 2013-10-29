{-# LANGUAGE TypeFamilies #-}
module Roguestar.Lib.Utility.DetailedTravel
    (Roguestar.Lib.Utility.DetailedTravel.whereIs,
     Roguestar.Lib.Utility.DetailedTravel.getContents)
    where

import Prelude hiding (getContents)
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Roguestar.Lib.DB as DB
import Roguestar.Lib.Utility.DetailedLocation

whereIs :: (DBReadable db,ReferenceType a) => Reference a -> db (DetailedLocation (Child a))
whereIs = liftM (fromMaybe (error "DetailedTravel.whereIs: Reference is not a child of it's own location.") . fromLocation) . asks . DB.whereIs

getContents :: (DBReadable db,ReferenceType a) => Reference a -> db [DetailedLocation (Parent a)]
getContents = liftM mapLocations . asks . DB.getContents

