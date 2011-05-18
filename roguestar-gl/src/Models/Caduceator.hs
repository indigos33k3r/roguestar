module Models.Caduceator
    (caduceator,
     caduceator_arm_upper,
     caduceator_arm_lower)
    where

import RSAGL.Math
import RSAGL.Math.CurveExtras
import RSAGL.Modeling
import Quality
import Models.Materials
import Models.Factions
import Models.FactionData

caduceator :: Faction -> Quality -> Modeling
caduceator f _ = rotate (Vector3D 0 0 1) (fromDegrees 180) $
                 rotateToFrom (Vector3D 0 0 1) (Vector3D 0 1 0) $ model $
    do model $
           do tube $ linearInterpolation
                  [(0,Point3D 0 0   (-5)),
	           (0.5,  Point3D 0 2   (-3)),
	           (0.5,  Point3D 0 2.5 (-2)),
	           (0.5,  Point3D 0 2.5 (-1)),
	           (1,    Point3D 0 1     2 ),
	           (1,    Point3D 0 0.8   4 ),
	           (1,    Point3D 0 1.2   5 ),
	           (0.5,  Point3D 0 3     6),
	           (0.5,  Point3D 0 4     5.5),
	           (0.5,  Point3D 0 7     3.5),
	           (0.5,  Point3D 0 7.5   4 ),
	           (0.75, Point3D 0 7.25  5 ),
	           (0,Point3D 0 7   7 )]
              deform $ \(Point3D x y z) -> 
                  let x' = flip lerpMap y
                        [(0,4),
	                 (1,5),
	                 (2,4),
	                 (3,2),
	                 (5,1),
	                 (6.9,1),
	                 (7,3),
	                 (8,1)]
	              in Point3D (x*x') y z
              material $ skin f caduceator_skin
       model $
           do sphere (Point3D 1.25 7.5 5) 0.25
	      sphere (Point3D (-1.25) 7.5 5) 0.25
	      material $ eye f
       affine $ scale' (1/20)

caduceator_arm_upper :: Faction -> Quality -> Modeling
caduceator_arm_upper f _ = rotate (Vector3D 1 0 0) (fromDegrees 90) $ model $
    do model $ sor $ linearInterpolation $
           points2d [(0,0),
	             (1.0,1.0),
		     (0.5,10.0),
		     (0,10.5)]
       material $ skin f caduceator_skin
       affine $ scale' (1/10)

caduceator_arm_lower :: Faction -> Quality -> Modeling
caduceator_arm_lower f _ = rotate (Vector3D 1 0 0) (fromDegrees 90) $ model $
    do model $ sor $ linearInterpolation $
           points2d [(0,-0.5),
	             (0.5,0.0),
		     (0.25,9.0),
		     (1.25,10.0),
		     (1.0,10.25),
		     (0.75,9.75),
		     (0,9)]
       material $ skin f caduceator_skin
       affine $ scale' (1/10)
