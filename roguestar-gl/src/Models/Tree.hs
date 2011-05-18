module Models.Tree
    (leafy_blob, tree_branch)
    where

import RSAGL.Modeling
import RSAGL.Color.RSAGLColors
import RSAGL.Math

leafy_blob :: ModelingM ()
leafy_blob = model $
    do sphere (Point3D 0 0 0) 1.0
       material $ pigment $ pure forest_green

tree_branch :: ModelingM ()
tree_branch = model $
    do hemisphere (Point3D 0 0 0) (Vector3D 0 0 1) 1.0
       material $ pigment $ pure brown

