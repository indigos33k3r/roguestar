module Models.EnergyThings
    (energyCylinder) where

import Models.LibraryData
import RSAGL.Modeling
import RSAGL.Math
import Quality
import Models.Materials
import RSAGL.Color.RSAGLColors

energyCylinder :: EnergyColor -> Quality -> Modeling
energyCylinder c _ = model $
    do model $
           do closedCone (Point3D 0 0 0,1.0) (Point3D 0 1 0,1.0)
              material $ emissive $ pure $ energyColor c
       model $
           do closedCone (Point3D 0 0 0,0.75) (Point3D 0 1 0,0.75)
              material $ emissive $ pure $ white

