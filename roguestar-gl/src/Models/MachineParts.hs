module Models.MachineParts
    (machine_arm_lower,
     machine_arm_upper,
     thin_limb)
    where

import Quality
import RSAGL.Modeling
import RSAGL.Math.CurveExtras
import RSAGL.Math
import Models.Factions
import Models.FactionData

machine_arm_lower :: Faction -> Quality -> Modeling
machine_arm_lower f _ = scale' (1/4) $ rotate (Vector3D 1 0 0) (fromDegrees 90) $ model $
    do sor $ linearInterpolation $ reverse $
        points2d [(0.0,4.5),
                  (0.25,4.5),
                  (0.25,3.5),
                  (0.35,3),
                  (0.35,0.5),
                  (0.0,0.3)]
       material $ metal f

machine_arm_upper :: Faction -> Quality -> Modeling
machine_arm_upper f _ = scale' (1/4) $ rotate (Vector3D 1 0 0) (fromDegrees 90) $ model $
    do sor $ linearInterpolation $ reverse $
        points2d [(0.0,4.5),
                  (0.5,4.5),
                  (0.75,3.5),
                  (0.5,3),
                  (0.5,1),
                  (0.0,-0.5)]
       material $ metal f

thin_limb :: Faction -> Quality -> Modeling
thin_limb f _ =
    do openCone (Point3D 0 0 0,0.05) (Point3D 0 0 1,0.05)
       sphere (Point3D 0 0 0) 0.05
       sphere (Point3D 0 0 1) 0.05
       material $ metal f
