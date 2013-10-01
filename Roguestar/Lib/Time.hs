module Roguestar.Lib.Time
    (actionTime,
     moveActionTime)
    where

import Roguestar.Lib.DB
import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.Position
import Data.Ratio
import Control.Monad

getBaseSpeed :: (DBReadable db) => MonsterRef -> db Integer
getBaseSpeed creature_ref =
    do c <- dbGetMonster creature_ref
       let raw_speed = rawScore Speed c
       when (raw_speed <= 0) $ error $ "getBaseSpeed: Non-positive raw speed (" ++ show c ++ ")"
       return raw_speed

-- | Time required to do a simple physical task.
actionTime :: (DBReadable db) => MonsterRef -> db Rational
actionTime creature_ref =
    do raw_speed <- getBaseSpeed creature_ref
       return $ 1000 % (1000 + raw_speed)

-- | Time required to move between tiles. Accepts a multiplier (i.e. 2x for half speed)
moveActionTime :: (DBReadable db) => Rational -> (Position,Position) -> MonsterRef -> db Rational
moveActionTime x (a,b) monster_ref =
    do raw_speed <- getBaseSpeed monster_ref
       let base_time = x * (100 % (100+raw_speed))
       let distance = sqrt $ fromIntegral $ distanceBetweenSquared a b :: Double
       return $ case () of
            () | distance >= 1.0 -> toRational distance * base_time
            () | otherwise -> base_time -- this really should not happen, but whatever

