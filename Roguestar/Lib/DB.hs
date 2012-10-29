{-# LANGUAGE MultiParamTypeClasses,
             ExistentialQuantification,
             FlexibleContexts,
             Rank2Types,
             RelaxedPolyRec,
             ScopedTypeVariables,
             TypeFamilies #-}

--Core
module Roguestar.Lib.DB
    (DB,
     runDB,
     DBReadable(..),
     playerState,
     setPlayerState,
     getPlayerCreature,
     setPlayerCreature,
     SnapshotEvent(..),
     initial_db,
     DB_BaseType(db_error_flag),
     dbActionCount,
     dbAddCreature,
     dbAddPlane,
     dbAddTool,
     dbAddBuilding,
     dbUnsafeDeleteObject,
     dbGetCreature,
     dbGetPlane,
     dbGetTool,
     dbGetBuilding,
     dbModCreature,
     dbModPlane,
     dbModTool,
     dbModBuilding,
     dbUnwieldCreature,
     dbVerify,
     dbGetAncestors,
     whereIs,
     getContents,
     move,
     ro, atomic,
     logDB,
     mapRO, filterRO, sortByRO,
     dbGetTimeCoordinate,
     dbAdvanceTime,
     dbNextTurn,
     dbPushSnapshot,
     peepOldestSnapshot,
     popOldestSnapshot,
     hasSnapshot,
     module Roguestar.Lib.DBData,
     module Roguestar.Lib.DBErrorFlag,
     module Roguestar.Lib.Random)
    where

import Prelude hiding (getContents)
import Roguestar.Lib.DBPrivate
import Roguestar.Lib.DBData
import Roguestar.Lib.Reference
import Roguestar.Lib.CreatureData
import Roguestar.Lib.PlaneData
import Roguestar.Lib.BuildingData
import Roguestar.Lib.RNG
import Data.Map as Map
import Data.List as List
import qualified Roguestar.Lib.HierarchicalDatabase as HD
import Data.Maybe
import Roguestar.Lib.ToolData
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Control.Applicative
import Roguestar.Lib.TimeCoordinate
import Data.Ord
import Control.Monad.Random as Random
import Roguestar.Lib.Random
import Roguestar.Lib.Data.PlayerState
import Roguestar.Lib.DBErrorFlag
import Control.Parallel.Strategies
import System.IO.Unsafe
import Roguestar.Lib.Logging
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Unboxed as Vector
import qualified System.Random.MWC as MWC
import Data.Word

data DBContext s = DBContext {
    db_info    :: STRef s DB_BaseType,
    db_rng     :: STRef s RNG,
    db_mwc_rng :: STRef s (MWC.GenST s) }

data DB_BaseType = DB_BaseType { db_player_state :: PlayerState,
                                 db_next_object_ref :: Integer,
                                 db_creatures :: Map CreatureRef Creature,
                                 db_player_creature :: Maybe CreatureRef,
                                 db_planes :: Map PlaneRef Plane,
                                 db_tools :: Map ToolRef Tool,
                                 db_buildings :: Map BuildingRef Building,
                                 db_hierarchy :: HD.HierarchicalDatabase Location,
                                 db_time_coordinates :: Map (Reference ()) TimeCoordinate,
                                 db_error_flag :: String,
                                 db_prior_snapshot :: Maybe DB_BaseType,
                                 db_action_count :: Integer }
    deriving (Read,Show)

data DB a = DB { internalRunDB :: forall s. DBContext s -> ST s (Either DBError a) }

runDB :: DB a -> DB_BaseType -> IO (Either DBError (a,DB_BaseType))
runDB dbAction database =
    do rng <- randomIO
       (seed :: Vector.Vector Word32) <- MWC.withSystemRandom . MWC.asGenIO $ \gen ->
                   MWC.uniformVector gen 2
       return $ runST $
           do mwc_rng_ref <- newSTRef =<< MWC.initialize seed
              data_ref <- newSTRef database
              rng_ref <- newSTRef rng
              result <- internalRunDB dbAction (DBContext data_ref rng_ref mwc_rng_ref)
              database' <- readSTRef data_ref
              return $ case result of
                  Left err -> Left err
                  Right a -> Right (a,database')

instance Monad DB where
    return a = DB $ const $ return $ Right a
    k >>= m = DB $ \context ->
        do result <- internalRunDB k context
           case result of
               Left err -> return $ Left err
               Right a -> internalRunDB (m a) context
    fail s = DB $ \_ -> return $ Left $ DBError s

instance Functor DB where
    fmap = liftM

instance Applicative DB where
    pure = return
    (<*>) = ap

instance MonadState DB_BaseType DB where
    get = DB $ \context -> liftM Right $ readSTRef (db_info context)
    put db1 = DB $ \context ->
                do db0 <- readSTRef (db_info context)
                   writeSTRef (db_info context) $
                       db1 { db_action_count = succ $ db_action_count db0 }
                   return $ Right ()

instance MonadReader DB_BaseType DB where
    ask = get
    local modification actionM =
        do db <- get
           modify modification
           a <- catchError (liftM Right actionM) (return . Left)
           put db
           either throwError return a

instance MonadError DBError DB where
    throwError e = DB $ \_ -> return $ Left e
    catchError actionM handlerM = DB $ \h ->
        do result <- internalRunDB actionM h
           case result of
               Left err -> internalRunDB (handlerM err) h
               x -> return $ x

instance MonadRandom DB where
    getRandom = dbRandom random
    getRandoms = liftM randoms $ dbRandom Random.split
    getRandomR min_max = dbRandom $ randomR min_max
    getRandomRs min_max = liftM (randomRs min_max) $ dbRandom Random.split

dbRandom :: (RNG -> (a,RNG)) -> DB a
dbRandom rgen = DB $ \context ->
    do g0 <- readSTRef (db_rng context)
       let (x,g1) = rgen g0
       writeSTRef (db_rng context) g1
       return $ Right x

class (Monad db,MonadError DBError db,MonadReader DB_BaseType db,MonadRandom db,Applicative db) => DBReadable db where
    dbSimulate :: DB a -> db a
    dbPeepSnapshot :: (DBReadable db) => (forall m. DBReadable m => m a) -> db (Maybe a)
    uniform :: (Int,Int) -> db Int
    uniformVector :: Int -> (Int,Int) -> db (Vector.Vector Int)

instance DBReadable DB where
    dbSimulate = local id
    dbPeepSnapshot actionM =
        do m_snapshot <- gets db_prior_snapshot
           case m_snapshot of
               Just snapshot ->
                   do liftM Just $ local (const snapshot) $ dbSimulate actionM
               Nothing ->  return Nothing
    uniform range = DB $ \context ->
        do gen <- readSTRef (db_mwc_rng context)
           liftM Right $ MWC.uniformR range gen
    uniformVector n (a,b) = DB $ \ context ->
        do gen <- readSTRef (db_mwc_rng context)
           liftM (Right . Vector.map ((+a) . (`mod` (b-a)))) $ MWC.uniformVector gen n

logDB :: (DBReadable db) => String -> Priority -> String -> db ()
logDB l p s = return $! unsafePerformIO $ logM l p $ l ++ ": " ++ s

ro :: (DBReadable db) => (forall m. DBReadable m => m a) -> db a
ro db = dbSimulate db

filterRO :: (DBReadable db) => (forall m. DBReadable m => a -> m Bool) -> [a] -> db [a]
filterRO f xs = liftM (`using` parList rseq) $ filterM (dbSimulate . f) xs

mapRO :: (DBReadable db) => (forall m. DBReadable m => a -> m b) -> [a] -> db [b]
mapRO f xs = liftM (`using` parList rseq) $ mapM (dbSimulate . f) xs

sortByRO :: (DBReadable db,Ord b) => (forall m. DBReadable m => a -> m b) -> [a] -> db [a]
sortByRO f xs =
    liftM (List.map fst . sortBy (comparing snd)) $ mapRO (\x ->
         do y <- f x
            return (x,y)) xs

-- | Run action synthesized from a read-only action (prepare-execute pattern).
atomic :: (x -> DB ()) -> (forall m. DBReadable m => m x) -> DB x
atomic action ro_action =
    do x <- ro ro_action
       s <- dbSimulate $
           do action x
              s <- get
              return s
       put s
       return x

-- |
-- Generates an initial DB state.
--
initial_db :: DB_BaseType
initial_db = DB_BaseType {
    db_player_state = SpeciesSelectionState Nothing,
    db_next_object_ref = 0,
    db_creatures = Map.fromList [],
    db_player_creature = Nothing,
    db_planes = Map.fromList [],
    db_tools = Map.fromList [],
    db_buildings = Map.fromList [],
    db_hierarchy = HD.fromList [],
    db_error_flag = [],
    db_time_coordinates = Map.fromList [(genericReference the_universe, zero_time)],
    db_prior_snapshot = Nothing,
    db_action_count = 0 }

playerState :: (DBReadable m) => m PlayerState
playerState = asks db_player_state

setPlayerState :: PlayerState -> DB ()
setPlayerState player_state = modify (\db -> db { db_player_state = player_state })

getPlayerCreature :: (DBReadable m) => m CreatureRef
getPlayerCreature = liftM (fromMaybe $ error "No player creature selected yet.") $ asks db_player_creature

setPlayerCreature :: CreatureRef -> DB ()
setPlayerCreature creature_ref = modify (\db -> db { db_player_creature = Just creature_ref })

dbActionCount :: (DBReadable db) => db Integer
dbActionCount = asks db_action_count

-- |
-- Gets the next ObjectRef integer, after incrementing it.
--
dbNextObjectRef :: DB Integer
dbNextObjectRef = do modify $ \db -> db { db_next_object_ref = succ $ db_next_object_ref db }
                     gets db_next_object_ref

-- |
-- Adds something to a map in the database using a new object reference.
--
dbAddObjectComposable :: (ReferenceType a) =>
                         (Integer -> (Reference a)) ->
                         (Reference a -> a -> DB ()) ->
                         (Reference a -> l -> Location) ->
                         a -> l -> DB (Reference a)
dbAddObjectComposable constructReferenceAction updateObjectAction constructLocationAction thing loc =
    do ref <- liftM constructReferenceAction $ dbNextObjectRef
       updateObjectAction ref thing
       setLocation $ constructLocationAction ref loc
       genericParent_ref <- liftM parentReference $ whereIs ref
       dbSetTimeCoordinate (genericReference ref) =<< dbGetTimeCoordinate (genericReference genericParent_ref)
       return ref

-- |
-- Adds a new Creature to the database.
--
dbAddCreature :: (LocationConstructor l, ReferenceTypeOf l ~ Creature) => Creature -> l -> DB CreatureRef
dbAddCreature = dbAddObjectComposable CreatureRef dbPutCreature (\r l -> constructLocation r l Nothing)

-- |
-- Adds a new Plane to the database.
--
dbAddPlane :: (LocationConstructor l, ReferenceTypeOf l ~ Plane) => Plane -> l -> DB PlaneRef
dbAddPlane = dbAddObjectComposable PlaneRef dbPutPlane (\r l -> constructLocation r l Nothing)

-- |
-- Adds a new Tool to the database.
--
dbAddTool :: (LocationConstructor l, ReferenceTypeOf l ~ Tool) => Tool -> l -> DB ToolRef
dbAddTool = dbAddObjectComposable ToolRef dbPutTool (\r l -> constructLocation r l Nothing)

-- |
-- Adds a new Tool to the database.
--
dbAddBuilding :: (LocationConstructor l, ReferenceTypeOf l ~ Building) => Building -> l -> DB BuildingRef
dbAddBuilding = dbAddObjectComposable BuildingRef dbPutBuilding (\r l -> constructLocation r l Nothing)

-- |
-- This deletes an object, which will cause future references to the same object
-- to fail.  Accepts a function to move all of the objects nested within the
-- object being deleted.
--
dbUnsafeDeleteObject :: (LocationConstructor l, ReferenceTypeOf l ~ ()) =>
    Reference e ->
    (forall m. (DBReadable m) => Reference () -> m l) ->
    DB ()
dbUnsafeDeleteObject ref f =
    do _ <- moveAllWithin ref f
       modify $ \db -> db {
           db_creatures = Map.delete (unsafeReference ref) $ db_creatures db,
           db_planes = Map.delete (unsafeReference ref) $ db_planes db,
           db_tools = Map.delete (unsafeReference ref) $ db_tools db,
           db_hierarchy = HD.delete (toUID ref) $ db_hierarchy db,
           db_time_coordinates = Map.delete (genericReference ref) $ db_time_coordinates db }

-- |
-- Puts an object into the database using getter and setter functions.
--
dbPutObjectComposable :: (Ord a) => (DB_BaseType -> Map a b) ->
                                    (Map a b -> DB_BaseType -> DB_BaseType) ->
                                    a -> b ->
                                    DB ()
dbPutObjectComposable get_map_fn put_map_fn key thing =
    modify (\db -> put_map_fn (Map.insert key thing $ get_map_fn db) db)

-- |
-- Puts a Creature under an arbitrary CreatureRef.
--
dbPutCreature :: CreatureRef -> Creature -> DB ()
dbPutCreature = dbPutObjectComposable db_creatures (\x db_base_type ->
     db_base_type { db_creatures = x })

-- |
-- Puts a Plane under an arbitrary PlaneRef
--
dbPutPlane :: PlaneRef -> Plane -> DB ()
dbPutPlane = dbPutObjectComposable db_planes $
    \x db_base_type -> db_base_type { db_planes = x }

-- |
-- Puts a Tool under an arbitrary ToolRef
--
dbPutTool :: ToolRef -> Tool -> DB ()
dbPutTool = dbPutObjectComposable db_tools $
    \x db_base_type -> db_base_type { db_tools = x }

-- |
-- Puts a Building under an arbitrary BuildingRef
--
dbPutBuilding :: BuildingRef -> Building -> DB ()
dbPutBuilding = dbPutObjectComposable db_buildings $
    \x db_base_type -> db_base_type { db_buildings = x }

-- |
-- Gets an object from the database using getter functions.
--
dbGetObjectComposable :: (DBReadable db) => String -> (DB_BaseType -> Map (Reference a) b) -> Reference a -> db b
dbGetObjectComposable type_info get_fn ref = 
    asks (fromMaybe (error $ "dbGetObjectComposable: Nothing.  UID was " ++ show (toUID ref) ++ ", type info was " ++ type_info) . Map.lookup ref . get_fn)

-- |
-- Gets a Creature from a CreatureRef
--
dbGetCreature :: (DBReadable m) => CreatureRef -> m Creature
dbGetCreature = dbGetObjectComposable "CreatureRef" db_creatures

-- |
-- Gets a Plane from a PlaneRef
--
dbGetPlane :: (DBReadable m) => PlaneRef -> m Plane
dbGetPlane = dbGetObjectComposable "PlaneRef" db_planes

-- |
-- Gets a Plane from a PlaneRef
--
dbGetTool :: (DBReadable m) => ToolRef -> m Tool
dbGetTool = dbGetObjectComposable "ToolRef" db_tools

-- |
-- Gets a Plane from a PlaneRef
--
dbGetBuilding :: (DBReadable m) => BuildingRef -> m Building
dbGetBuilding = dbGetObjectComposable "BuildingRef" db_buildings

-- |
-- Modifies an Object based on an ObjectRef.
--
dbModObjectComposable :: (Reference e -> DB e) -> (Reference e -> e -> DB ()) ->
                         (e -> e) -> Reference e -> DB ()
dbModObjectComposable getter putter f ref = (putter ref . f) =<< (getter ref)

-- |
-- Modifies a Plane based on a PlaneRef.
--
dbModPlane :: (Plane -> Plane) -> PlaneRef -> DB ()
dbModPlane = dbModObjectComposable dbGetPlane dbPutPlane

-- |
-- Modifies a Creature based on a PlaneRef.
--
dbModCreature :: (Creature -> Creature) -> CreatureRef -> DB ()
dbModCreature = dbModObjectComposable dbGetCreature dbPutCreature

-- |
-- Modifies a Tool based on a PlaneRef.
--
dbModTool :: (Tool -> Tool) -> ToolRef -> DB ()
dbModTool = dbModObjectComposable dbGetTool dbPutTool

-- |
-- Modifies a Tool based on a PlaneRef.
--
dbModBuilding :: (Building -> Building) -> BuildingRef -> DB ()
dbModBuilding = dbModObjectComposable dbGetBuilding dbPutBuilding

-- | A low-level set location instruction.  Merely guarantees the consistency of the location graph.
setLocation :: Location -> DB ()
setLocation loc =
    do logDB log_database DEBUG $ "setting location: " ++ show loc
       case loc of
           IsWielded _ (Wielded c) -> dbUnwieldCreature c
           IsSubsequent _ (Subsequent s v) -> shuntPlane (\subseq -> subsequent_via subseq == v) s
           IsBeneath _ (Beneath b) -> shuntPlane (\(Beneath {}) -> True) b
           _ -> return ()
       modify (\db -> db { db_hierarchy = HD.insert loc $ db_hierarchy db })

-- |
-- Bump any existing child Plane matching the predicate to TheUniverse.
-- Used to guarantee that a Plane can have only one child of a particular type.
-- (only one plane Beneath, only one plane Subsequent via a particular type of gateway).
--
shuntPlane :: (LocationDetail a) => (a -> Bool) -> PlaneRef -> DB ()
shuntPlane f p =
    do locations <- liftM (List.filter (maybe False f . fromLocation)) $ getContents p
       mapM_ (maybe (return ()) setLocation . shuntToTheUniverse) locations

-- |
-- Shunt any wielded objects into inventory.
--
dbUnwieldCreature :: CreatureRef -> DB ()
dbUnwieldCreature c = mapM_ (maybe (return ()) setLocation . returnToInventory) =<< getContents c

-- |
-- Moves an object, returning the location of the object before and after
-- the move.
--
move :: (LocationConstructor l, ReferenceTypeOf l ~ e) => Reference e -> l -> DB (Location,Location)
move ref location_data =
    do old <- whereIs ref
       let new = constructLocation ref location_data (Just old)
       setLocation new
       when (childReference old /= childReference new) $
           throwError $ DBError "moveTo: Object changed identity during move!"
       when (parentReference old == parentReference new) $
           dbSetTimeCoordinate ref =<< dbGetTimeCoordinate (parentReference new)
       return (old,new)

moveAllWithin :: (LocationConstructor l, ReferenceTypeOf l ~ ()) =>
                 Reference e ->
                 (forall m. (DBReadable m) => Reference () -> m l) ->
                 DB [(Location,Location)]
moveAllWithin ref f =
    do all_entities <- liftM (List.map childReference) $ getContents ref
       forM all_entities $ \e -> move e =<< f e

-- |
-- Verifies that a reference is in the database.
--
dbVerify :: (DBReadable db) => Reference e -> db Bool
dbVerify ref = asks (isJust . HD.parentOf (toUID ref) . db_hierarchy)

whereIs :: (DBReadable db) => Reference e -> db Location
whereIs item = asks (fromMaybe (error "whereIs: has no location") . HD.lookupParent (toUID item) . db_hierarchy)

-- |
-- Returns all ancestor Locations of this element starting with the location
-- of the element and ending with TheUniverse.
--
dbGetAncestors :: (DBReadable db) => Reference e -> db [Location]
dbGetAncestors ref | genericReference ref == genericReference the_universe = return []
dbGetAncestors ref =
    do this <- whereIs ref
       rest <- dbGetAncestors $ parentReference this
       return $ this : rest

-- |
-- Returns locations of all children of a reference.
--
getContents :: (DBReadable db) => Reference t -> db [Location]
getContents item = asks (HD.lookupChildren (toUID item) . db_hierarchy)

-- |
-- Gets the time of an object.
--
dbGetTimeCoordinate :: (DBReadable db,ReferenceType a) => Reference a -> db TimeCoordinate
dbGetTimeCoordinate ref = asks (fromMaybe (error "dbGetTimeCoordinate: missing time coordinate.") . 
                                  Map.lookup (genericReference ref) . db_time_coordinates)

-- |
-- Sets the time of an object.
--
dbSetTimeCoordinate :: Reference a -> TimeCoordinate -> DB ()
dbSetTimeCoordinate ref tc = modify (\db -> db { db_time_coordinates = Map.insert (genericReference ref) tc $ db_time_coordinates db })

-- |
-- Advances the time of an object.
--
dbAdvanceTime :: (ReferenceType a) => Reference a -> Rational -> DB ()
dbAdvanceTime ref t = dbSetTimeCoordinate ref =<< (return . (advanceTime t)) =<< dbGetTimeCoordinate ref

-- |
-- Finds the object whose turn is next, among a restricted group of objects.
--
dbNextTurn :: (DBReadable db,ReferenceType a) => [Reference a] -> db (Reference a)
dbNextTurn [] = error "dbNextTurn: empty list"
dbNextTurn refs =
    do logDB log_database INFO $ "Determining whose turn is next among: " ++ (show $ List.map toUID refs) 
       asks (\db -> fst $ minimumBy (comparing snd) $
                   List.map (\r -> (r,fromMaybe (error "dbNextTurn: missing time coordinate") $
                                      Map.lookup (genericReference r) (db_time_coordinates db))) refs)

-- |
-- Takes a snapshot of a SnapshotEvent in progress.
--
dbPushSnapshot :: SnapshotEvent -> DB ()
dbPushSnapshot e = modify $ \db -> db {
    db_prior_snapshot = Just $ db { db_player_state = SnapshotEvent e } }

peepOldestSnapshot :: (DBReadable db) => (forall m. DBReadable m => m a) -> db a
peepOldestSnapshot actionM =
    do m_a <- dbPeepSnapshot $ peepOldestSnapshot actionM
       maybe actionM return m_a

popOldestSnapshot :: DB ()
popOldestSnapshot = modify popOldestSnapshot_

hasSnapshot :: (DBReadable db) => db Bool
hasSnapshot = liftM isJust $ dbPeepSnapshot (return ())

popOldestSnapshot_ :: DB_BaseType -> DB_BaseType
popOldestSnapshot_ db =
    case isJust $ db_prior_snapshot =<< db_prior_snapshot db of
        False -> db { db_prior_snapshot = Nothing }
        True  -> db { db_prior_snapshot = fmap popOldestSnapshot_ $ db_prior_snapshot db }

