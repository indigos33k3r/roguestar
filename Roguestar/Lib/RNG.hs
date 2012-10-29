--Services

-- |
-- Don't depend on any external source of psuedo-random numbers, because
-- we want to be able to save a psuedo-random seed and know that we can
-- generate the same psuedo-random sequence when we reload it, even across
-- different environments.
--
module Roguestar.Lib.RNG
    (mkRNG,
     RNG,
     Random(..),
     RandomGen(..),
     blurp)
    where

import System.Random
import Control.Arrow (first)

newtype RNG = RNG { rng_state :: Integer }

k :: Integer
k = 281474976710656

max_range :: (Integral i) => i
max_range = 16777216

instance RandomGen RNG where
    next g = (fromInteger $ x `quot` max_range,RNG x)
        where x = (rng_state g * 0x5DEECE66D + 0xB) `mod` k
    split g = (mkRNG $ fromIntegral x,mkRNG $ fromIntegral y)
        where (x,g') = next g
              (y,_) = next g'
    genRange _ = (0,max_range)

instance Random RNG where
    random = first (mkRNG :: Integer -> RNG) . random
    randomR _ = random

-- |
-- Construct an RNG from a seed.
--
mkRNG :: (Integral i) => i -> RNG
mkRNG = RNG . fromIntegral . fst . next . RNG . toInteger

blurp :: Integer -> Integer
blurp x = (x * 0x5DEECE66D + 0xB) `mod` k

