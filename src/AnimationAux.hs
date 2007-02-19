{-# OPTIONS_GHC -fglasgow-exts #-}

module AnimationAux 
    (newStepAnimation,
     newLerpAnimation,
     newAcceleratedLerpAnimation,
     animGetAnswer,
     animGetTable,
     DataFreshness(..))
    where
    
import AnimationCore
import Globals
import Time
import Math3D
import Control.Monad
import Data.IORef
import Driver
import Tables
    
type GetA a o = AniM () o (Maybe a)
type AToO a o = (a,o) -> a -> AniM () o o
type RenderO a o = (a,o) -> AniM () o ()
type LerpTime o = o -> o -> AniM () o Time
    
stepAnimation :: (Eq a) => a -> o -> GetA a o -> AToO a o -> RenderO a o -> () -> AniM () o o
stepAnimation a o getA atoO renderO _ = 
    do newa <- getA
       case newa of
               Just newa' | a /= newa' -> do newo <- atoO (a,o) newa'
                                             animHardSwitch () $ stepAnimation newa' newo getA atoO renderO
               _ -> do renderO (a,o)
                       return o

-- |
-- A generic step animation.  
-- newStepAnimation a o getA atoO renderO
-- a is a representation of the object to be rendered.  The animation thread watches the value of a returned by getA.
-- If it changes, atoO is run to generate a new value for o, which will be returned until a changes again.
-- atoO recieves the old values of a and o in its first parameter and the value of a in its second parameter.
--
newStepAnimation :: (Eq a) => a -> o -> GetA a o -> AToO a o -> RenderO a o -> IO (Animation () o)
newStepAnimation a o getA atoO renderO = newAnimation $ stepAnimation a o getA atoO renderO

data LerpAnimation o = LerpAnimation { lerpanim_start :: Time,
                                       lerpanim_duration :: Time,
                                       lerpanim_old :: LerpAnimation o,
                                       lerpanim_new :: o }
                     | LerpAnimationStill o -- since we lerp recursively against old (interrupted) lerps, keep an alternate representation of completed lerps
                                            -- don't lerp recursively against every lerp we've ever done.

-- |
-- Retrieve the most recent un-interpolated value of a LerpAnimation.
--
lerpAnimNewest :: LerpAnimation o -> o
lerpAnimNewest (LerpAnimationStill o) = o
lerpAnimNewest o = lerpanim_new o

animLerp :: (Lerpable a) => LerpAnimation a -> (Double -> Double) -> AniM i o a
animLerp (LerpAnimationStill x) _ = return x
animLerp x lerpMutator = 
    do oldx <- animLerp (lerpanim_old x) lerpMutator
       time <- animTime
       return $ lerp (lerpMutator $ toSeconds $ max 0 $ min 1 $ (time - lerpanim_start x) / (max 0.1 $ lerpanim_duration x)  :: Double) (oldx,lerpanim_new x)

optimizeLerp :: LerpAnimation a -> AniM i o (LerpAnimation a)
optimizeLerp x@(LerpAnimationStill _) = return x
optimizeLerp x@(LerpAnimation {}) = 
    do lerp_is_done <- liftM (lerpanim_start x + lerpanim_duration x <) animTime
       if lerp_is_done
           then return $ LerpAnimationStill $ lerpanim_new x
           else liftM (\old -> x { lerpanim_old = old }) $ optimizeLerp $ lerpanim_old x

lerpAnimation :: (Eq a,Lerpable o) => a -> LerpAnimation o -> GetA a o -> AToO a o -> RenderO a o -> LerpTime o -> (Double -> Double) -> () -> AniM () o o
lerpAnimation a o getA atoO renderO lerpTimeO lerpMutator _ = 
    do newa <- getA
       secs <- animTime
       case newa of
               Just newa' | a /= newa' -> do newo <- atoO (a,lerpAnimNewest o) newa'
                                             lerp_time <- lerpTimeO (lerpAnimNewest o) newo
                                             o' <- optimizeLerp o
                                             let newo' = LerpAnimation { lerpanim_start = secs,
                                                                         lerpanim_duration = lerp_time,
                                                                         lerpanim_old = o',
                                                                         lerpanim_new = newo }
                                             animHardSwitch () $ lerpAnimation newa' newo' getA atoO renderO lerpTimeO lerpMutator
               _ -> do lerped_o <- animLerp o lerpMutator
                       renderO (a,lerped_o)
                       return lerped_o

-- |
-- A generic linear-interpolated animation.  This works the same as newStepAnimation, with a new function,
-- lerpTime, which indicates how long the linear interpolation should take.  For example, this might be the
-- pythagorean distance between two points, or just a constant value.
--
newLerpAnimation :: (Eq a,Lerpable o) => a -> o -> GetA a o -> AToO a o -> RenderO a o -> LerpTime o -> IO (Animation () o)
newLerpAnimation a o getA atoO renderO lerpTime = newAnimation $ lerpAnimation a (LerpAnimationStill o) getA atoO renderO lerpTime id

-- |
-- Another linear-interpolated animation.  This time, the interpolation is non-linear wrt time.  Intead, the animated
-- object accelerates to a maximum speed halfway through the interpolation, and the slows as it approaches the end.
-- No change should be needed to replace newLerpAnimation with newAcceleratedLerpAnimation.
--
newAcceleratedLerpAnimation :: (Eq a,Lerpable o) => a -> o -> GetA a o -> AToO a o -> RenderO a o -> LerpTime o -> IO (Animation () o)
newAcceleratedLerpAnimation a o getA atoO renderO lerpTime = newAnimation $ lerpAnimation a (LerpAnimationStill o) getA atoO renderO (accelerateTime lerpTime) accelerateLerp
    where accelerateLerp x | x < 0.5 = 0.5 * (2 * x) ^ 2
          accelerateLerp x = 1.0 - 0.5 * (2 * (1-x)) ^ 2
          accelerateTime fn x y = (return . (fromSeconds . (* 2) . sqrt . toSeconds)) =<< fn x y

-- |
-- driverGetAnswer, embedded in the Animation Monad.
--
animGetAnswer :: IORef RoguestarGlobals -> DataFreshness -> String -> AniM i o (Maybe String)
animGetAnswer globals_ref freshness s = AnimationCore.unsafeAnimIO $ driverGetAnswer globals_ref freshness s

-- |
-- driverGetTable, embedded in the Animation Monad.
--
animGetTable :: IORef RoguestarGlobals -> DataFreshness -> String -> String -> AniM i o (Maybe RoguestarTable)
animGetTable globals_ref freshness the_table_name the_table_id = 
        AnimationCore.unsafeAnimIO $ driverGetTable globals_ref freshness the_table_name the_table_id