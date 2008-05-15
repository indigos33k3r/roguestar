\section{Pre-specified Colors, Models, Materials and Deformations}

\begin{code}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module RSAGL.ModelingExtras
    (gray,
     gray256,
     smoothbox,
     regularPrism,
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
     ColorFunction,
     Pattern,
     module RSAGL.RSAGLColors,
     module RSAGL.Material,
     module RSAGL.ApplicativeWrapper,
     module Control.Applicative)
    where

import Graphics.Rendering.OpenGL.GL.BasicTypes
import RSAGL.Noise
import RSAGL.RSAGLColors
import Control.Applicative
import RSAGL.ApplicativeWrapper
import RSAGL.Vector
import RSAGL.Material
import RSAGL.Affine
import RSAGL.Model
import System.Random
import RSAGL.Interpolation
import Data.Monoid
import RSAGL.Auxiliary
import RSAGL.Angle
\end{code}

\subsection{Colors}

\texttt{RSAGL.ModellingSupport} exports the contents of \texttt{rsagl-colors.txt}.  This file is translated to HTML and Haskell source by the script in \texttt{ProcessColors.hs}.  Color samples can be viewed by opening the file \texttt{rsagl-colors.html} in a web browser.

With the exception of \texttt{blackbody}, all of the colors contain non-zero values for the red, green, and blue components, so that extremely bright lights will (realistically) wash those colors out to white.

\begin{code}
gray :: Float -> RGB
gray x = rgb x x x

gray256 :: (Integral i) => i -> RGB
gray256 x = rgb256 x x x
\end{code}

\subsection{Models}

\texttt{smoothbox} is a box that takes an extra smoothing parameter between 0 and 1.  This box doesn't have perfectly flat normals, and may therefore be a little easier on the eye.

\begin{code}
smoothbox :: (Monoid attr) => Double -> Point3D -> Point3D -> Modeling attr
smoothbox u p q = model $
    do box p q
       deform $ \(SurfaceVertex3D point vector) -> SurfaceVertex3D point $ vectorNormalize $ lerp u (vector,vectorNormalize $ vectorToFrom point midpoint)
        where midpoint = lerp 0.5 (p,q)
\end{code}

\texttt{regularPrism} constructs a regular n-sided prism or pyramid.

\begin{code}
regularPrism ::(Monoid attr) => (Point3D,Double) -> (Point3D,Double) -> Integer -> Modeling attr
regularPrism (a,ra) (b,rb) n = 
    model $ translate (vectorToFrom a origin_point_3d) $ 
        rotateToFrom (Vector3D 0 1 0) (vectorToFrom b a) $ sequence_ $ rotationGroup (Vector3D 0 1 0) n $ quad
  where a1 = Point3D 0 0 ra
        a2 = rotateY (fromRotations $ recip $ fromInteger n) a1
        b1 = Point3D 0 (distanceBetween a b) rb
        b2 = rotateY (fromRotations $ recip $ fromInteger n) b1
        quad = quadralateral a1 a2 b2 b1
\end{code}

\texttt{rotationGroup} rotates a model repeatedly.

\begin{code}
rotationGroup :: (AffineTransformable a) => Vector3D -> Integer -> a -> [a]
rotationGroup v n m = map (flip (rotate v) m . fromRotations) $ tail $ zeroToOne (n+1)
\end{code}

\subsection{Patterns}

\texttt{cloudy} is a pattern made using perlin noise.  \texttt{spherical} is a pattern that ranges from the center of a sphere to its radius, where the center maps to zero and the radius maps to one.  \texttt{directional} is a pattern based on the directional (infinite) light source.  An object rendered with an emissive layer defined by a directional light source will seem to be lit from that direction.

\begin{code}
type ColorFunction a = ApplicativeWrapper ((->) SurfaceVertex3D) a

type Pattern = SurfaceVertex3D -> Double

pattern :: (ColorClass a) => Pattern -> [(GLfloat,ColorFunction a)] -> ColorFunction a
pattern _ [(_,constant_pattern)] = constant_pattern
pattern f color_map = wrapApplicative (\sv3d -> toApplicative (lerpMap color_map $ realToFrac $ f sv3d) $ sv3d)

cloudy :: Int -> Double -> Pattern
cloudy seed wave_length (SurfaceVertex3D p _) = perlinNoise (translate offset $ scale' frequency p) + 0.5
    where frequency = recip wave_length
          offset = vectorNormalize $ fst $ randomXYZ (-1000.0*wave_length,1000.0*wave_length) (mkStdGen seed)

blinkBoxes :: Int -> Double -> Double -> Double -> Pattern
blinkBoxes seed box_size chaos threshold = thresholdF . cloudy seed (recip chaos) . toLatticeCoordinates
    where thresholdF u = if u > threshold then 1.0 else 0.0
          toLatticeCoordinates (SurfaceVertex3D (Point3D x y z) v) = 
              SurfaceVertex3D (Point3D (to1LatticeCoordinate x) (to1LatticeCoordinate y) (to1LatticeCoordinate z)) v
          to1LatticeCoordinate u = realToFrac $ round $ u/box_size

spherical :: Point3D -> Double -> Pattern
spherical center radius (SurfaceVertex3D p _) = distanceBetween center p / radius

directional :: Vector3D -> Pattern
directional vector (SurfaceVertex3D _ v) = dotProduct (vectorNormalize v) normalized_vector
    where normalized_vector = vectorNormalize vector

gradient :: Point3D -> Vector3D -> Pattern
gradient center vector (SurfaceVertex3D p _) = dotProduct vector (vectorToFrom p center) / vectorLengthSquared vector
\end{code}

\subsection{Materials}

\begin{code}
glass :: RGBFunction -> MaterialM attr ()
glass rgbf = 
    do transparent $ (alpha 0.05) <$> rgbf
       specular 100 $ (\rgb_color -> curry (lerp (brightness rgb_color)) rgb_color white) <$> rgbf

plastic :: RGBFunction -> MaterialM attr ()
plastic rgbf = 
    do pigment rgbf
       specular 50 (pure white)

metallic :: RGBFunction -> MaterialM attr ()
metallic rgbf = 
    do pigment rgbf
       specular 75 rgbf
\end{code}

\subsection{Deformations}

\texttt{bumps} can be used to describe any deformation in which vertices are purturbed in the direction of their normal vectors.

\texttt{waves} is a deformation that makes little waves in a surface.

\begin{code}
bumps :: Pattern -> Modeling attr
bumps f = deform $ (\(sv3d@(SurfaceVertex3D p v)) -> translate (vectorScale (f sv3d) v) p)

waves :: Double -> Double -> Pattern
waves wave_length amplitude (SurfaceVertex3D (Point3D x y z) _) = (wave_f x + wave_f y + wave_f z) * amplitude / 3
    where wave_f u = sin (u / wave_length * 2*pi)
\end{code}