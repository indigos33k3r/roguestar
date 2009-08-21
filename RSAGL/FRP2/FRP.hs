{-# LANGUAGE ExistentialQuantification, Arrows, EmptyDataDecls, RecursiveDo, ScopedTypeVariables, Rank2Types #-}

module RSAGL.FRP2.FRP
    (FRPX,
     FRP1,
     Threaded,
     FRP,
     switchContinue,
     switchTerminate,
     spawnThreads,
     killThreadIf,
     threadIdentity,
     frpTest,
     FRPProgram,
     newFRPProgram,
     newFRP1Program,
     updateFRPProgram,
     integral,
     summation,
     derivative,
     integralRK4,
     integralRK4',
     absoluteTime,
     threadTime,
     frpContext,
     frp1Context,
     whenJust)
    where

import Prelude hiding ((.),id)
import RSAGL.FRP2.FactoryArrow
import Control.Monad.Cont
import RSAGL.FRP.Time
import Control.Concurrent.MVar
import Control.Category
import Control.Arrow
import Data.IORef
import Control.Applicative
import RSAGL.Math.AbstractVector
import RSAGL.Math.RK4
import Data.List
import Data.Maybe

{--------------------------------------------------------------------------------}
--    FRP Data Structures
{--------------------------------------------------------------------------------}

-- | State information for a currently-executed FRP program.
data FRPState i o = FRPState { 
                               -- | Ending time of the current frame, and the frame-local time horizon.
                               frpstate_absolute_time :: Time,
                               -- | The ending time of the previous frame.
                               frpstate_delta_time :: Time,
                              -- | Continuation to exit the current switch.
                               frpstate_exit :: (Maybe o) -> ContT (Maybe o) IO (Maybe o) }

data FRPInit t i o = FRPInit {
    frp_current_switch :: IORef (i -> ContT (Maybe o) IO (Maybe o)),
    frp_state :: IORef (FRPState i o),
    -- | Put a thread in here to spawn it.
    frp_spawned_threads :: MVar [FRPInit t i o],
    frp_previous_time :: IORef (Maybe Time),
    frp_thread_identity :: t,
    frp_previous_result :: IORef (Maybe o) }

type FRPProgram i o = FRPInit () i o

-- | An FRPX program that allows threading.
data Threaded

-- | An FRP program that allows only anonymous threading.
type FRP = FRPX Threaded ()
-- | An FRP program that forbids threading.
type FRP1 = FRPX () ()

-- | A switchable automata with timewise numerical methods.
-- k is the threading parameter, either Threaded or ().
-- t is the type of the thread ID (for example, a unique integer).
-- i is the type of the switch input.
-- o is the type of the switch output.
-- j is the type of the immediate arrow input.
-- p is the type of the immediate arrow output.
newtype FRPX k t i o j p = FRPX (FRPInit t i o -> FactoryArrow IO (ContT (Maybe o) IO) j p)

instance Functor (FRPX k t i o j) where
    fmap f frpx = frpx >>> arr f

instance Applicative (FRPX k t i o j) where
    pure a = proc _ -> returnA -< a
    f <*> s = proc i ->
        do s' <- s -< i
           f' <- f -< i
           returnA -< f' s'

instance Category (FRPX k t i o) where
    (FRPX a) . (FRPX b) = FRPX $ \frp_init -> a frp_init . b frp_init
    id = FRPX $ const id

instance Arrow (FRPX k t i o) where
    arr f = FRPX $ \_ -> arr f
    first (FRPX f) = FRPX $ \frp_init -> first (f frp_init)
    second (FRPX f) = FRPX $ \frp_init -> second (f frp_init)

-- | Construct a single-threaded FRPProgram.
newFRP1Program :: FRPX () () i o i o -> IO (FRPProgram i o)
newFRP1Program = unsafeFRPProgram ()

-- | Construct a multi-threaded FRPProgram.
newFRPProgram :: [(t,FRPX Threaded t i o i o)] -> IO (FRPProgram i [(t,o)])
newFRPProgram seed_threads = newFRP1Program $ frpContext seed_threads

-- | Construct a multi-threaded FRPProgram from a single seed thread.
unsafeFRPProgram :: t -> FRPX k t i o i o -> IO (FRPInit t i o)
unsafeFRPProgram t frpx =
    do frpstate_ref <- newIORef (error "Tried to use uninitialized FRPState variable.")
       current_switch_ref <- newIORef (error "Tried to use uninitialized frp_current_switch variable.")
       spawned_threads <- newEmptyMVar
       previous_time_ref <- newIORef Nothing
       previous_result_ref <- newIORef Nothing
       let frp_init = FRPInit current_switch_ref frpstate_ref spawned_threads previous_time_ref t previous_result_ref
       writeIORef current_switch_ref =<< constructSwitch frp_init frpx
       return frp_init

-- | Bring an FRPProgram up-to-date with the current time.
updateFRPProgram :: i -> FRPProgram i o -> IO o
updateFRPProgram i frp_init =
    do t <- getTime
       liftM (fromMaybe $ error "updateFRPProgram: unexpected termination") $ unsafeRunFRPProgram t i frp_init

frpTest :: [FRPX Threaded () i o i o] -> [i] -> IO [[o]]
frpTest seed_threads inputs =
    do test_program <- newFRPProgram $ map (\thread -> ((),thread)) seed_threads
       liftM (map $ map snd . fromMaybe (error "frpTest: unexpected termination")) $ 
            mapM (\(t,i) -> unsafeRunFRPProgram t i test_program) $ zip (map fromSeconds [0.0,0.1..]) inputs

-- | Update an FRPProgram.
unsafeRunFRPProgram :: Time -> i -> FRPInit t i o -> IO (Maybe o)
unsafeRunFRPProgram t i frp_init =
    do prev_t <- readIORef (frp_previous_time frp_init)
       let state = FRPState {
                       frpstate_absolute_time = t,
                       frpstate_delta_time = fromMaybe zero $ sub <$> pure t <*> prev_t,
                       frpstate_exit = \o ->
               do lift $ writeIORef (frp_previous_time frp_init) $ Just t
                  lift $ writeIORef (frp_previous_result frp_init) $ o
                  return o }
       writeIORef (frp_state frp_init) state
       action <- readIORef (frp_current_switch frp_init)
       runContT (action i) return

getFRPState :: FRPInit t i o -> IO (FRPState i o)
getFRPState = readIORef . frp_state

-- | Shorthand for simple operations in the ContT monad.
frpxOf :: (FRPInit t i o -> j -> ContT (Maybe o) IO p) -> FRPX k t i o j p
frpxOf action = FRPX $ \frpinit -> FactoryArrow $ return $ Kleisli $ action frpinit

-- | Framewise accumulation of signals.
-- The embedded function recieves the current input and the previous output.
accumulate :: p -> (j -> p -> p) -> FRPX k t i o j p
accumulate initial_value accumF = FRPX $ \_ -> FactoryArrow $
    do prev_o_ref <- newIORef initial_value
       return $ Kleisli $ \i -> lift $
           do prev_o <- readIORef prev_o_ref
              let o = accumF i prev_o
              writeIORef prev_o_ref o
              return o

-- | Delay a piece of data for one frame.
delay :: x -> FRPX k t i o x x
delay initial_value = accumulate (initial_value,error "delay: impossible") (\new_value (old_value,_) -> (new_value,old_value)) >>> arr snd

-- | Take the integral of a rate over time, using the trapezoidal rule.
integral :: (AbstractVector v,AbstractAdd p v) => p -> FRPX k t i o (Rate v) p
integral initial_value = proc v ->
    do delta_t <- deltaTime -< ()
       (new_accum,_) <- accumulate (zero,perSecond zero) (\(delta_t,new_rate) (old_accum,old_rate) ->
           (old_accum `add` ((scalarMultiply (recip 2) $ new_rate `add` old_rate) `over` delta_t),new_rate)) -< (delta_t,v)
       returnA -< initial_value `add` new_accum

-- | Take the derivative of a value over time, by simple subtraction between frames.
derivative :: (AbstractVector v,AbstractSubtract p v) => FRPX k t i o p (Rate v)
derivative = proc new_value ->
    do delta_t <- deltaTime -< ()
       m_old_value <- delay Nothing -< Just new_value
       let z = perSecond zero
       returnA -< maybe z (\old_value -> if delta_t == zero then z else (new_value `sub` old_value) `per` delta_t) m_old_value

-- | 'accumulate' harness for some numerical methods.
-- Parameters are: current input, previous output, delta time, absolute time, and number of frames at the specified frequency.
accumulateNumerical :: Frequency -> (i -> o -> Time -> Time -> Integer -> o) -> o -> FRPX any t x y i o
accumulateNumerical frequency accumF initial_value = proc i ->
    do absolute_time <- absoluteTime -< ()
       delta_t <- deltaTime -< ()
       accumulate initial_value (\(i,absolute_time',delta_t',frames) o -> accumF i o absolute_time' delta_t' frames) -< 
           (i,absolute_time,delta_t,ceiling $ toSeconds delta_t / toSeconds (interval frequency))

integralRK4 :: (AbstractVector v) => Frequency -> (p -> v -> p) -> p -> FRPX any t i o (Time -> p -> Rate v) p
integralRK4 f addPV = accumulateNumerical f (\diffF p abs_t delta_t -> integrateRK4 addPV diffF p (abs_t `sub` delta_t) abs_t)

integralRK4' :: (AbstractVector v) => Frequency -> (p -> v -> p) -> (p,Rate v) -> 
                FRPX any t x y (Time -> p -> Rate v -> Acceleration v) (p,Rate v)
integralRK4' f addPV = accumulateNumerical f (\diffF p abs_t delta_t -> integrateRK4' addPV diffF p (abs_t `sub` delta_t) abs_t)

-- | Sum some data frame-by-frame.
summation :: (AbstractAdd p v) => p -> FRPX k t i o v p
summation initial_value = accumulate initial_value (\v p -> p `add` v)

-- | Elapsed time since the instantiation of this switch or thread.  Reset when a thread switches.
threadTime :: FRPX k t i o () Time
threadTime = summation zero <<< threadTime

-- | Get the current absolute time.  This value is transferable across different
-- instances of an application and even between different computers and operating
-- systems (assuming a correctly set system clock).
absoluteTime :: FRPX k t i o () Time
absoluteTime = frpxOf $ \frpinit () -> lift $ do liftM frpstate_absolute_time $ getFRPState frpinit

-- | Get the change in time since the last update.
deltaTime :: FRPX k t i o () Time
deltaTime = frpxOf $ \frpinit () -> lift $ do liftM frpstate_delta_time $ getFRPState frpinit

-- | Replace the 'frpinit_current_switch' value of the currently running thread with
-- a newly constructed switch.
replaceSwitch :: FRPInit t i o -> FRPX k t i o i o -> ContT (Maybe o) IO (i -> ContT (Maybe o) IO (Maybe o))
replaceSwitch frpinit switch =
    do newSwitch <- lift $ constructSwitch frpinit switch
       lift $ writeIORef (frp_current_switch frpinit) newSwitch
       return newSwitch

constructSwitch :: FRPInit t i o -> FRPX k t i o i o -> IO (i -> ContT (Maybe o) IO (Maybe o))
constructSwitch frp_init (FRPX f) =
    do (Kleisli current_switch) <- runFactory $ f frp_init
       return $ liftM Just . current_switch

-- | Whenever a value is provided, change the presently running switch (or thread) to the specified new value,
-- and execute that switch before continuing.  This destroys all state local to the currently running
-- switch (or thread).
-- This function acts as if the switch were performed at frameBegin.
switchContinue :: FRPX k t i o (Maybe (FRPX k t i o i o),i) i
switchContinue = frpxOf $ \frpinit (m_switch,i) ->
    do case m_switch of
           (Just switch) ->
               do newSwitch <- replaceSwitch frpinit switch
                  newSwitch i
                  error "switchContinue: Unreachable code."
           Nothing -> return i

-- | Whenever a value is provided, change the presently running switch (or thread) to the specified new value,
-- and execute that switch before continuing.  This destroys all state local to the currently running
-- switch (or thread).
-- This function acts as if the switch were performed at frameEnd.
switchTerminate :: FRPX k t i o (Maybe (FRPX k t i o i o),o) o
switchTerminate = frpxOf $ \frp_init (m_switch,o) ->
    do case m_switch of
           (Just switch) ->
               do replaceSwitch frp_init switch
                  exit <- lift $ liftM frpstate_exit $ getFRPState frp_init
                  exit $ Just o
                  error "switchTerminate: Unreachable code."
           Nothing -> return o

-- | Spawn new threads once per frame.
spawnThreads :: FRPX Threaded t i o [(t,FRPX Threaded t i o i o)] ()
spawnThreads = frpxOf $ \frp_init new_threads -> lift $
    do let spawned_threads_var = frp_spawned_threads frp_init
       constructed_new_threads <- mapM (liftM (\t -> t { frp_spawned_threads = frp_spawned_threads frp_init }) . uncurry unsafeFRPProgram) new_threads
       modifyMVar_ spawned_threads_var $ return . (constructed_new_threads ++)
       return ()

-- | Kill the current thread, only when the given parameter is true.
killThreadIf :: FRPX Threaded t i o Bool ()
killThreadIf = frpxOf $ \frpinit b ->
    do exit <- lift $ liftM frpstate_exit $ getFRPState frpinit
       when b $ exit Nothing >> return ()
       return ()

-- | Get the current thread's identity. 
threadIdentity :: FRPX k t i o () t
threadIdentity = frpxOf $ \frpinit () -> return $ frp_thread_identity frpinit

data ThreadGroup t i o = ThreadGroup {
    thread_outputs :: [ThreadResult t i o],
    thread_group :: MVar [FRPInit t i o] }

data ThreadResult t i o = ThreadResult {
    thread_output :: o,
    thread_object :: FRPInit t i o }

threadResults :: ThreadGroup t i o -> [(t,o)]
threadResults = map (\t -> (frp_thread_identity $ thread_object t,thread_output t)) . thread_outputs

-- | A complex function that embeds a thread group inside another running thread.  If the parent thread terminates
-- or switches, the embedded thread group is instantly lost.
--
-- 'threadGroup' accepts two paremters:
-- * A multithreaded algorithm.  The simplest (and single-threaded) implementation is sequence_.
-- * A list of seed threads with their associated thread identities.
unsafeThreadGroup :: forall t u j p k l i o. ([IO ()] -> IO ()) -> [(t,FRPX k t j p j p)] -> FRPX l u i o j (ThreadGroup t j p)
unsafeThreadGroup multithread seed_threads = FRPX $ \frp_init -> FactoryArrow $
   mdo threads <- newMVar =<< mapM (liftM (\t -> t { frp_spawned_threads = threads }) . uncurry unsafeFRPProgram) seed_threads
       let runThreads :: j -> IO [ThreadResult t j p]
           runThreads j =
               do threads_this_pass <- takeMVar threads
                  putMVar threads []
                  absolute_time <- liftM frpstate_absolute_time $ getFRPState frp_init
                  multithread $ map (\t -> unsafeRunFRPProgram absolute_time j t >> return ()) threads_this_pass
                  results_this_pass <- liftM catMaybes $ forM threads_this_pass $ \t ->
                      do m_o <- readIORef (frp_previous_result t)
                         return $
                             do o <- m_o
                                return $ ThreadResult o t
                  results <- liftM (results_this_pass++) (if null results_this_pass then return [] else runThreads j)
                  modifyMVar_ threads (return . ((map thread_object results_this_pass)++))
                  return results
       return $ Kleisli $ \j ->
           do results <- lift $ runThreads j
              return $ ThreadGroup {
                  thread_outputs = results,
                  thread_group = threads }

-- | Embed some threads inside another running thread, as 'threadGroup'.
frpContext :: [(t,FRPX Threaded t j p j p)] -> FRPX k u i o j [(t,p)]
frpContext seed_threads = arr threadResults . unsafeThreadGroup sequence_ seed_threads

-- | Embed a single-threaded, bracketed switch inside another running thread.
frp1Context :: FRPX () () j p j p -> FRPX k t i o j p
frp1Context thread = proc i ->
    do os <- unsafeThreadGroup sequence_ [((),thread)] -< i
       returnA -< case threadResults os of
           [((),o)] -> o
           _ -> error "frp1Context: unexpected non-singular result."

whenJust :: (forall x y. FRPX () () x y j p) -> FRPX k t i o (Maybe j) (Maybe p)
whenJust actionA = frp1Context whenJust_
    where whenJust_ = proc i ->
              do switchContinue -< (maybe (Just whenNothing_) (const Nothing) i,i)
	         arr (Just) <<< actionA -< fromMaybe (error "whenJust: impossible case") i
	  whenNothing_ = proc i ->
	      do switchContinue -< (fmap (const whenJust_) i,i)
	         returnA -< Nothing

