{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

-- Mechanics
module Roguestar.Lib.Contact
    (findContacts,
     ContactMode(..),
     ContactModeType(..))
    where

import Prelude hiding (getContents)
import Roguestar.Lib.Position as Position
import Roguestar.Lib.Facing
import Roguestar.Lib.DB
import Roguestar.Lib.CreatureData
import Control.Monad
import Roguestar.Lib.PlaneData
import Data.Ord
import Data.List as List
import Roguestar.Lib.DetailedLocation

-- | 'Touch' contacts are on the same or facing square as the subject.
-- 'Line' contacts are on any point starting on the same square and anywhere directly along a line traced in the
-- facing direction, out to infinity.  'Area' contacts lie inside a circle of radius 7, centered 7 squares in the
-- facing direction.  Use 'Area' 'Here' for a circle centered on the subject.
data ContactMode = Touch | Line | Area

class ContactModeType a where
    contactMode :: a -> ContactMode

instance ContactModeType ContactMode where
    contactMode = id

instance ContactModeType CreatureInteractionMode where
    contactMode Unarmed = Touch
    contactMode Melee = Touch
    contactMode Ranged = Line
    contactMode Splash = Area

-- | Find contacts to a reference.  The result is sorted by from closest to
-- farthest from the subject, except in the case of area contacts, which are
-- sorted from the center of the area.  The subject is never a contact of
-- itself.
findContacts :: (DBReadable db,ContactModeType c) =>
                c -> Reference x -> Facing -> db [DetailedLocation Planar]
findContacts contact_mode attacker_ref face =
    do (m_l :: Maybe (Parent Plane,MultiPosition)) <- liftM fromLocation $ whereIs attacker_ref
       let testF pos (x :: MultiPosition) = case contactMode contact_mode of
               Touch -> min (x `distanceBetweenChessboard` (offsetPosition (facingToRelative face) pos))
                            (x `distanceBetweenChessboard` pos) == 0
               Line -> isFacing (pos,face) x
               Area -> Position.distanceBetweenSquared (offsetPosition (facingToRelative7 face) pos) x < 49
           center_pos pos = case contactMode contact_mode of
               Area -> offsetPosition (facingToRelative7 face) pos
               _ -> pos
       flip (maybe $ return []) m_l $ \(Parent plane_ref,pos) ->
           liftM (sortBy (comparing (Position.distanceBetweenSquared (center_pos pos) . (detail :: DetailedLocation Planar -> MultiPosition))) .
                  filter ((/= genericReference attacker_ref) . asChild . detail) .
                  filter (testF pos . detail)) $
                      (liftM mapLocations $ getContents plane_ref)


