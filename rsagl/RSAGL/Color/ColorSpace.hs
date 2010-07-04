module RSAGL.Color.ColorSpace
    (ColorSpace(..),
     ImportColorCoordinates(..),
     ExportColorCoordinates(..),
     AffineColorSpace,
     ColorWheel,
     ColorChannel,
     ChannelIndex,
     LinearMetric(..),
     newChannel,
     newAngularChannel,
     viewChannel,
     channel_u,
     channel_v,
     channel_w,
     newColorSpace,
     newColorWheel,
     color_space_rgb,
     color_wheel_rgbl,
     transformColorFromTo,
     transformColor)
    where

import RSAGL.Types
import RSAGL.Math.Matrix
import RSAGL.Math.Vector
import RSAGL.Math.AbstractVector
import RSAGL.Math.Affine
import Data.Vec (nearZero)
import RSAGL.Math.Angle

-- | An affine transformation of the default RGB color space.
newtype AffineColorSpace = AffineColorSpace Matrix
    deriving (Show)

-- | A rotatable color space.
newtype ColorWheel = ColorWheel Matrix
    deriving (Show)

-- | A specific component of a 3-channel color space.
newtype ColorChannel = LinearChannel Matrix
    deriving (Show)

-- | 'channel_u', 'channel_v', 'channel_w', of a 3-channel color space.
data ChannelIndex = ChannelIndex { channel_index :: Matrix }

-- | The first channel of a color space represented by the ordered tripple,
-- @(u,v,w)@.
channel_u :: ChannelIndex
channel_u = ChannelIndex identity_matrix

-- | The second channel of a color space represented by the ordered tripple,
-- @(u,v,w)@.
channel_v :: ChannelIndex
channel_v = ChannelIndex $ matrix
    [[0,1,0,0],
     [1,0,0,0],
     [0,0,1,0],
     [0,0,0,1]]

-- | The third channel of a color space represented by the ordered tripple,
-- @(u,v,w)@.
channel_w :: ChannelIndex
channel_w = ChannelIndex $ matrix
   [[0,0,1,0],
    [0,1,0,0],
    [1,0,0,0],
    [0,0,0,1]]

-- | Pick a channel from a color space.
newChannel :: (ColorSpace cs) => ChannelIndex -> cs -> ColorChannel
newChannel (ChannelIndex ch_ix) cs = LinearChannel $ m `matrixMultiply` ch_ix
    where (AffineColorSpace m) = affineColorSpaceOf cs

-- | Construct a 'ColorChannel' that runs along a hue angle.
-- The meaning of the hue angle depends on the primary colors
-- used in the construction of the color wheel.
newAngularChannel :: ColorWheel -> Angle -> ColorChannel
newAngularChannel (ColorWheel m) a = LinearChannel $
        m
    `matrixMultiply`
        rotationMatrix (Vector3D 0 0 1) (scalarMultiply (-1) a)

-- | A view of a specific color channel, such as red, or luminance.
data LinearMetric = LinearMetric {
    -- | The range of a color channel that is within gamut.
    -- This range depends on the channel and the particular
    -- color being observed, and may not exist if the
    -- color itself is out of gamut.
    linear_gamut_bounds :: Maybe (RSdouble,RSdouble),
    -- | A function to modify a color channel independantly
    -- from the other color channels in the same color space.
    linear_color_function :: RSdouble -> Color,
    -- | The value of the particular color channel for the
    -- particular color.
    linear_value :: RSdouble }

instance Show LinearMetric where
    show x = show (linear_gamut_bounds x, linear_value x)

-- | Read a specific channel of a color.
viewChannel :: (ExportColorCoordinates c) =>
                  ColorChannel -> c -> LinearMetric
viewChannel (LinearChannel m) c = LinearMetric {
              linear_gamut_bounds = case intersections of
                  [] -> Nothing
                  is -> Just (minimum is,maximum is),
              linear_color_function = \x -> importColorCoordinates $
                  transformColorFromTo (AffineColorSpace m) (Point3D x v w),
              linear_value = u }
    where Point3D u v w = exportColorCoordinates c $ AffineColorSpace m
          rgb@(Point3D r g b) = exportColorCoordinates c color_space_rgb
          rgb'@(Vector3D r' g' b') = colorVectorOf (AffineColorSpace m)
                                                   (Vector3D 1 0 0)
          intersections = map (+u) $ filter gamutValid $
               map (uncurry (/)) $ filter (not . nearZero . snd)
                   [(1-r,r'),(1-g,g'),(1-b,b'),
                    (-r,r'),(-g,g'),(-b,b')]
          colorFunction x = rgb `add` scalarMultiply x rgb'
          gamutValid x = let Point3D r'' g'' b'' = colorFunction x
              in (>=3) $ length $ filter (>= (-0.001)) $ filter (<= 1.001) $ [r'',g'',b'']

-- | A color space specification or color type that has an associated
-- color space.
--
-- If a type implements both 'ImportColorCoordinates' and
-- 'ColorSpace', then it must ensure that:
--
-- @importColorCoordinates f =
--     (let c = importColorCoordinates (const $ f $ affineColorSpaceOf c) in c)@
--
-- This is not hard -- all that is required is that
-- @affineColorSpaceOf undefined@ is defined.
--
class ColorSpace cs where
    affineColorSpaceOf :: cs -> AffineColorSpace

-- | A color type that can export its color coordinates.
-- An easy implementation is
--
-- @transformColorFromTo your_color_space your_color_coordinates@
--
-- If a type implements both 'ExportColorCoordinates' and
-- 'ImportColorCoordinates', then it must ensure that
-- @importColorCoordinates . exportColorCoordinates = id@.
--
class ExportColorCoordinates c where
    exportColorCoordinates ::
        c -> AffineColorSpace -> Point3D

-- | A color type that can import its color coordinates.
class ImportColorCoordinates c where
    importColorCoordinates ::
        (AffineColorSpace -> Point3D) -> c

instance ColorSpace AffineColorSpace where
    affineColorSpaceOf = id

instance ColorSpace ColorWheel where
    affineColorSpaceOf (ColorWheel m) = AffineColorSpace m

-- | A generic representation of Color.
newtype Color = Color { fromColor ::
    AffineColorSpace -> Point3D }

instance ExportColorCoordinates Color where
    exportColorCoordinates = fromColor

instance ImportColorCoordinates Color where
    importColorCoordinates = Color

-- | Construct a new color space.  This requires a minimal point
-- (the black point in an additive color space, or the white point
-- in a subtractive color space), and three primary colors.
-- The three primarys color correspond to the 'channel_u',
-- 'channel_v', and 'channel_w' respectively.
newColorSpace :: (ExportColorCoordinates c) =>
    c -> c -> c -> c -> AffineColorSpace
newColorSpace k u v w = AffineColorSpace $
    xyzMatrix (u' `sub` k')
              (v' `sub` k')
              (w' `sub` k')
    where k' = exportColorCoordinates k color_space_rgb
          u' = exportColorCoordinates u color_space_rgb
          v' = exportColorCoordinates v color_space_rgb
          w' = exportColorCoordinates w color_space_rgb

-- | Construct a new color wheel.  This requires a minimal point,
-- (the black point in an additive color space, or the white point
-- in a subtractive color space), and three primary colors with
-- assigned hue angles and value parameters.
-- The hue angle maps onto 'channel_u' and 'channel_v', while
-- the value parameter maps directly and additively onto
-- 'channel_w'.
newColorWheel :: (ExportColorCoordinates c) =>
    c -> (c,Angle,RSdouble) ->
         (c,Angle,RSdouble) ->
         (c,Angle,RSdouble) ->
         ColorWheel
newColorWheel k (u,theta_u,u') (v,theta_v,v') (w,theta_w,w') =
        ColorWheel $ uvw_to_rgb `matrixMultiply` matrixInverse uvw_to_wheel
    where uvw_to_wheel = matrix $
              [ [cosine theta_u, cosine theta_v, cosine theta_w, 0 ],
                [  sine theta_u,   sine theta_v,   sine theta_w, 0 ],
                [            u',             v',             w', 0 ],
                [             0,              0,              0, 1 ] ]
          AffineColorSpace uvw_to_rgb = newColorSpace k u v w

-- | A color wheel constructed with red, green and blue device primaries
-- and a luminance component.  This is the basis of the HCL color system.
{-# NOINLINE color_wheel_rgbl #-}
color_wheel_rgbl :: ColorWheel
color_wheel_rgbl = ColorWheel $ matrixInverse $ matrix $
  [ [1.0    , -0.5       , -0.5                  , 0.0],
    [0.0    , (sqrt 3/2) , (negate $ sqrt 3 / 2) , 0.0],
    [0.2126 , 0.7152     , 0.0722                , 0.0],
    [0.0    , 0.0        , 0.0                   , 1.0] ]

-- | The red-green-blue device color space.
{-# NOINLINE color_space_rgb #-}
color_space_rgb :: AffineColorSpace
color_space_rgb = AffineColorSpace $ identity_matrix

-- | Transform ordered triples between color spaces.
transformColorFromTo :: AffineColorSpace ->
                        Point3D ->
                        AffineColorSpace ->
                        Point3D
transformColorFromTo (AffineColorSpace source)
                     uvw
                     (AffineColorSpace destination) =
    transform (matrixInverse destination `matrixMultiply` source)
              uvw

-- | Transform a color vector into RGB space.
colorVectorOf :: AffineColorSpace ->
                 Vector3D ->
                 Vector3D
colorVectorOf (AffineColorSpace m) uvw =
    transform m uvw

{-# RULES
"transformColor::a->a"    transformColor = id
  #-}
-- | Transform colors between color spaces.
transformColor :: (ExportColorCoordinates source,
                   ImportColorCoordinates dest) =>
                  source -> dest
transformColor = importColorCoordinates . exportColorCoordinates

