module Models.Hellion
    (hellion,
     hellion_appendage,
     hellion_eye)
    where

import RSAGL.Math
import RSAGL.Modeling
import Quality
import Models.Materials
import RSAGL.Math.CurveExtras
import RSAGL.Color.RSAGLColors
import Models.Factions
import Models.FactionData

hellion :: Faction -> Quality -> Modeling
hellion f _ = model $
    do sphere (Point3D 0 0 0.6) 0.1
       material $ skin f hellion_skin

hellion_appendage :: Faction -> Quality -> Modeling
hellion_appendage f _ = model $ rotate (Vector3D 1 0 0) (fromDegrees 90) $
    do sor $ linearInterpolation $
           points2d [(0  ,0),
                     (0.02,0.25),
                     (0.03,0.5),
                     (0.02,0.75),
                     (0.0,1.0)]
       material $ skin f hellion_skin

hellion_eye :: Faction -> Quality -> Modeling
hellion_eye f _ = model $
    do model $
           do openCone (Point3D 0 0 (-0.1),0) (Point3D 0 0 0.1, 0.1)
              material $ skin f hellion_skin
       model $
           do hemisphere (Point3D 0 0 0.1) (Vector3D 0 0 1.0) 0.1
              material $ pigment $ pure white
       model $
           do perspectiveSphere (Point3D 0 0 0.1) 0.103 (Point3D 0 0 0.25)
              material $ iris f
              twoSided True
       model $
           do perspectiveSphere(Point3D 0 0 0.1) 0.106 (Point3D 0 0 0.22)
              material $ eye f
              twoSided True

