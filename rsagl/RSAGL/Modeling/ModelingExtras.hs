{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module RSAGL.Modeling.ModelingExtras
    (smoothbox,
     regularPrism,
     heightField,
     heightDisc,
     rotationGroup,
     glass,
     plastic,
     metallic,
     pattern,
     cloudy,
     blinkBoxes,
     spherical,
     directional,
     gradient,
     bumps,
     waves,
     heightMap,
     disregardSurfaceNormals,
     ColorFunction,
     Pattern,
     dropRandomElements,
     module RSAGL.Modeling.Material,
     module RSAGL.Auxiliary.ApplicativeWrapper,
     module Control.Applicative)
    where

import RSAGL.Modeling.Noise
import RSAGL.Color
import RSAGL.Color.RSAGLColors
import Control.Applicative
import RSAGL.Auxiliary.ApplicativeWrapper
import RSAGL.Math.Vector
import RSAGL.Modeling.Material
import RSAGL.Math.Affine
import RSAGL.Modeling.Model
import System.Random
import RSAGL.Math.Interpolation
import Data.Monoid
import RSAGL.Auxiliary.Auxiliary
import RSAGL.Math.Angle
import RSAGL.Math.Ray
import RSAGL.Math.AbstractVector
import RSAGL.Math.Types

smoothbox :: RSdouble -> Point3D -> Point3D -> Modeling
smoothbox u p q = model $
    do box p q
       deform $ \(SurfaceVertex3D point vector) -> SurfaceVertex3D point $ vectorNormalize $ lerp u (vector,vectorNormalize $ vectorToFrom point midpoint)
        where midpoint = lerp 0.5 (p,q)

regularPrism ::(Point3D,RSdouble) -> (Point3D,RSdouble) -> Integer -> Modeling
regularPrism (a,ra) (b,rb) n = 
    model $ translate (vectorToFrom a origin_point_3d) $ 
        rotateToFrom (Vector3D 0 1 0) (vectorToFrom b a) $ sequence_ $ rotationGroup (Vector3D 0 1 0) n $ quad
  where a1 = Point3D 0 0 ra
        a2 = rotateY (fromRotations $ recip $ fromInteger n) a1
        b1 = Point3D 0 (distanceBetween a b) rb
        b2 = rotateY (fromRotations $ recip $ fromInteger n) b1
        quad = quadralateral a1 a2 b2 b1

-- | A rectangular height field rising off of the x-z plane.
heightField :: (RSdouble,RSdouble) -> (RSdouble,RSdouble) -> ((RSdouble,RSdouble) -> RSdouble) -> Modeling
heightField (x1,z1) (x2,z2) f = model $
    do quadralateral (Point3D x1 0 z1) (Point3D x1 0 z2) (Point3D x2 0 z2) (Point3D x2 0 z1)
       heightMap f
       

-- | A circular height field rising off of the x-z plane.
heightDisc :: (RSdouble,RSdouble) -> RSdouble -> ((RSdouble,RSdouble) -> RSdouble) -> Modeling
heightDisc (x,y) r f = model $
    do closedDisc (Point3D x 0 y) (Vector3D 0 1 0) r
       heightMap f

rotationGroup :: (AffineTransformable a) => Vector3D -> Integer -> a -> [a]
rotationGroup v n m = map (flip (rotate v) m . fromRotations) $ tail $ zeroToOne (n+1)

-- | Defines a color as a function of a point on a surface.
-- A simple ColorFunction might be 'pure red'.
type ColorFunction a = ApplicativeWrapper ((->) SurfaceVertex3D) a

-- | Defines variation as a function of a point on a surface,
-- without describing the particular colors.  For example I
-- might combine the 'cloudy' Pattern with the colors blue and white
-- to make a cloudy sky sphere.
type Pattern = SurfaceVertex3D -> RSdouble

-- | Combines a pattern with a a list of color functions.
-- At any given point on a model, we will take the result
-- of the 'Pattern' function at that point and interpolate
-- between the two nearest ColorFunctions.
pattern :: (AbstractVector a) => Pattern -> [(RSfloat,ColorFunction a)] -> ColorFunction a
pattern _ [(_,constant_pattern)] = constant_pattern
pattern f color_map = wrapApplicative (\sv3d -> toApplicative (lerpMap color_map $ f2f $ f sv3d) $ sv3d)

-- | A smooth pseudo-random cloudy pattern.
-- First parameter is a random seed.  Second parameter
-- is the size of the clouds.
cloudy :: Int -> RSdouble -> Pattern
cloudy seed wave_length (SurfaceVertex3D p _) = perlinNoise (translate offset $ scale' frequency p) + 0.5
    where frequency = recip wave_length
          offset = vectorNormalize $ fst $ randomXYZ (-1000.0*wave_length,1000.0*wave_length) (mkStdGen seed)

-- | Randomly alternating squares (cubes).  Always returns 1 or 0.
-- First parameter is a random seed.  Second parameter
-- is the size of the boxes.  Third parameter is the degree of randomness
-- between adjacent boxes, with adjacent boxes likely to have
-- the same color for low values of chaos.  Final parameter determines
-- what fraction of boxes are which color, with 0.5 representing
-- a 50/50 chance.
blinkBoxes :: Int -> RSdouble -> RSdouble -> RSdouble -> Pattern
blinkBoxes seed box_size chaos threshold = thresholdF . cloudy seed (recip chaos) . toLatticeCoordinates
    where thresholdF u = if u > threshold then 1.0 else 0.0
          toLatticeCoordinates (SurfaceVertex3D (Point3D x y z) v) = 
              SurfaceVertex3D (Point3D (to1LatticeCoordinate x) (to1LatticeCoordinate y) (to1LatticeCoordinate z)) v
          to1LatticeCoordinate u = fromInteger $ round $ u/box_size

-- | A pattern describing distance from a center point.
-- The pattern may be given a radius, which expands or shrinks
-- the entire pattern proportionally.
spherical :: Point3D -> RSdouble -> Pattern
spherical center radius (SurfaceVertex3D p _) = distanceBetween center p / radius

-- | A pattern determined by the surface normal of a point on a model.
-- Among other things, this could be used to paint fake illumination
-- on a model.
directional :: Vector3D -> Pattern
directional vector (SurfaceVertex3D _ v) = dotProduct (vectorNormalize v) normalized_vector
    where normalized_vector = vectorNormalize vector

-- | A pattern which changes along a straight line in one direction.
-- Like a layer cake.
gradient :: Point3D -> Vector3D -> Pattern
gradient center vector (SurfaceVertex3D p _) = distanceAlong (Ray3D center vector) p

-- | Quick way to construct a colored glass material.
glass :: RGBFunction -> MaterialM ()
glass rgbf =
    do transparent $ (alpha 0.05 . transformColor) <$> rgbf
       specular 100 $ (\rgb_color ->
           curry (lerp $ linear_value $ viewChannel channel_luminance rgb_color)
                      rgb_color white) <$> rgbf

-- | Quicky way to construct a colored plastic-like material.
plastic :: RGBFunction -> MaterialM ()
plastic rgbf =
    do pigment rgbf
       specular 50 (pure white)

-- | Quicky way to construct a colored metal-like material.
metallic :: RGBFunction -> MaterialM ()
metallic rgbf =
    do pigment rgbf
       specular 75 rgbf

-- | 'bumps' can be used to describe any deformation in which vertices are purturbed in the direction of their normal vectors.
bumps :: Pattern -> Modeling
bumps f = deform $ \(sv3d@(SurfaceVertex3D p v)) -> translate (vectorScale (f sv3d) v) p

waves :: RSdouble -> RSdouble -> Pattern
waves wave_length amplitude (SurfaceVertex3D (Point3D x y z) _) = (wave_f x + wave_f y + wave_f z) * amplitude / 3
    where wave_f u = sin (u / wave_length * 2*pi)

-- | Raises or lowers each point in a model along the y-axis according to its (x,z) coordinate.
-- Typically this is used to construct height fields.
heightMap :: ((RSdouble,RSdouble) -> RSdouble) -> Modeling
heightMap f = deform $ \(Point3D x y z) -> Point3D x (y + f (x,z)) z

-- | Turns off calculation of surface normals.  This can speed
-- up modeling in some cases if we know we don't need them.
disregardSurfaceNormals :: Modeling
disregardSurfaceNormals = deform $ \(SurfaceVertex3D p _) -> SurfaceVertex3D p (Vector3D 0 1 0)

