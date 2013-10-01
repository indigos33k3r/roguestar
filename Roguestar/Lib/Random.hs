
--Core
module Roguestar.Lib.Random
    (WeightedSet,
     weightedSet,
     unweightedSet,
     append,
     fromWeightedSet,
     weightedPick,
     weightedPickM,
     linearRoll,
     fixedSumRoll,
     fixedSumLinearRoll,
     logRoll,
     opposedLinearPowerRatio,
     rationalRoll)
    where

import Data.List as List
import System.Random ()
import Control.Monad.Random
import Control.Monad
import Data.Ratio
import Data.Ord
import qualified Data.Vector as Vector

data WeightedSet a = WeightedSet {
    weighted_set_total :: Integer,
    weighted_set :: Vector.Vector (Integer,a) }
        deriving (Read,Show)

weightedSet :: [(Integer,a)] -> WeightedSet a
weightedSet [] = error "Tried to pick from an empty list."
weightedSet as = WeightedSet {
    weighted_set_total = sum $ List.map fst as,
    weighted_set = Vector.fromList $ reverse $ sortBy (comparing fst) as }

unweightedSet :: [a] -> WeightedSet a
unweightedSet [] = error "Tried to pick from an empty list."
unweightedSet as = WeightedSet {
    weighted_set_total = genericLength as,
    weighted_set = Vector.fromList $ List.map (\x -> (1,x)) as }

append :: WeightedSet a -> WeightedSet a -> WeightedSet a
append a b = weightedSet $ (Vector.toList $ weighted_set a) ++ (Vector.toList $ weighted_set b)

instance Functor WeightedSet where
    fmap f s = WeightedSet (weighted_set_total s) $ Vector.map (\(x,y) -> (x,f y)) $ weighted_set s

fromWeightedSet :: WeightedSet a -> [a]
fromWeightedSet = List.map snd . Vector.toList . weighted_set

-- | Pick an element of a weighted list at random.  E.g. in "[(2,x),(3,y)]" "y" will be picked three times out of five while "x" will be picked 2 times out of five.
weightedPick :: (RandomGen g) => WeightedSet a -> g -> (a,g)
weightedPick elems = runRand (weightedPickM elems)

-- | 'weightedPick' in MonadRandom
weightedPickM :: (MonadRandom m) => WeightedSet a -> m a
weightedPickM elems =
    do weight_to_find <- getRandomR (1,weighted_set_total elems)
       return $ pickWithWeight weight_to_find 0 $ weighted_set elems

pickWithWeight :: Integer -> Int -> Vector.Vector (Integer,a) -> a
pickWithWeight i ix v = case Vector.unsafeIndex v ix of
    (x,_) | i > x -> pickWithWeight (i-x) (succ ix) v
    (_,a) | otherwise -> a

-- | Roll an (n+1) sided die numbered zero to n.
linearRoll :: (MonadRandom m) => Integer -> m Integer
linearRoll n = getRandomR (0,n)

-- | fixedSumRoll using 'linearRoll', with optimizations.
-- REVISIT: this can be improved significantly, but performance doesn't seem to be a material problem so far.
fixedSumLinearRoll :: (MonadRandom m) => [Integer] -> Integer -> m [Integer]
fixedSumLinearRoll xs a = fixedSumRoll (List.map (linearRoll . min a) xs) a

-- | Roll a sequence of random variables, such that the sum of the result is a fixed value.
fixedSumRoll :: (MonadRandom m) => [m Integer] -> Integer -> m [Integer]
fixedSumRoll rs a =
    do xs <- sequence rs
       case sum xs == a of
           True -> return xs
           False -> fixedSumRoll rs a

-- | Roll a die where the typical outcome is the base-2 logarithm of the input.
-- This function has exactly the same probability of rolling exactly 0 as 'linearDiceRoll'.
--
logRoll :: (MonadRandom m) => Integer -> m Integer
logRoll n = liftM (min n) $ accumRoll 0 n
    where accumRoll c x =
              do x' <- linearRoll x
                 case x' of
                     0 -> return c
                     _ -> accumRoll (c+1) x'

-- | Roll on a rational number that is a probability between zero and one, to generate a boolean.
rationalRoll :: (MonadRandom m) => Rational -> m Bool
rationalRoll r =
    do p <- linearRoll (denominator r - 1)
       return $ p < numerator r

-- | 'opposedLinearPowerRatio' is used when a constant (non-random) power relationship needs to be
-- determined between two parties.  (For example, this is used in the Spot/Hide contest when determining
-- line of sight.)
--
-- It accepts negative values for either parameter, and is invertable, i.e., 
-- @opposedLinearPowerRatio a b@ = @1 - opposedLinearPowerRatio b a@
--
-- One use is: @2 * (a%1) * opposedLinearPowerRatio a b@, whichs gives you roughly @a@ if @a@ and @b@ are equal,
-- or less or more than @a@ otherwise.
opposedLinearPowerRatio :: Integer -> Integer -> Rational
opposedLinearPowerRatio a b | a < 1 = opposedLinearPowerRatio 1 (b-a+1)
opposedLinearPowerRatio a b | b < 1 = opposedLinearPowerRatio (a-b+1) 1
opposedLinearPowerRatio a b | a >= b = ((a-b) % a) + (b % a)/2
opposedLinearPowerRatio a b | otherwise = 1 - opposedLinearPowerRatio b a


