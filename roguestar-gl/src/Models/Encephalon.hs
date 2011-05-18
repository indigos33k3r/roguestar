module Models.Encephalon
    (encephalon)
    where

import Quality
import RSAGL.Math
import RSAGL.Math.CurveExtras
import RSAGL.Modeling
import Models.Materials
import Models.Factions
import Models.FactionData

encephalon_head :: Faction -> Quality -> Modeling
encephalon_head f _ = model $
    do sor $ linearInterpolation $
           points2d $ reverse
                    [(0,9),
                     (0.5,9),
                     (1,9),
                     (1.5,9),
                     (2,9),
                     (3,8.5),
                     (4,7),
                     (4,5),
                     (3,3)]
       deform dfn
       material $ skin f encephalon_skin
  where dfn (Point3D x y z) = Point3D x (min (abs x ** 4 + 7.5) y) z

encephalon_eye :: Faction -> Quality -> Modeling
encephalon_eye f _ = model $
    do sphere origin_point_3d 0.4
       material $ eye f

encephalon_suit :: Faction -> Quality -> Modeling
encephalon_suit f _ = model $
    do sor $ linearInterpolation $
           points2d $ reverse
                    [(3,5),
                     (5,5),
                     (6,3),
                     (8,1),
                     (8,0.5),
                     (7,0),
                     (0,0)]
       material $ metal f

encephalon :: Faction -> Quality -> Modeling
encephalon f q = rotate (Vector3D 0 0 1) (fromDegrees 180) $
                 rotateToFrom (Vector3D 0 0 1) (Vector3D 0 1 0) $ model $ scale' (1/30) $
    do encephalon_head f q
       encephalon_suit f q
       translate (Vector3D (-1) 6 4) $ encephalon_eye f q
       translate (Vector3D 1 6 4) $ encephalon_eye f q

