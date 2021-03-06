-- Utilities
module Roguestar.Lib.Utility.HierarchicalDatabase
    (HierarchicalDatabase,
     HierarchicalRelation(..),
     Roguestar.Lib.Utility.HierarchicalDatabase.empty,
     Roguestar.Lib.Utility.HierarchicalDatabase.insert,
     Roguestar.Lib.Utility.HierarchicalDatabase.delete,
     Roguestar.Lib.Utility.HierarchicalDatabase.lookup,
     lookupChildren,
     lookupParent,
     parentOf,
     childrenOf,
     Roguestar.Lib.Utility.HierarchicalDatabase.toList,
     Roguestar.Lib.Utility.HierarchicalDatabase.fromList,
     testcases)
    where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe as Maybe
import qualified Test.HUnit as HUnit

-- | A record that can be a component of a 'HierarchicalDatabase'.
class HierarchicalRelation a where
    parent :: a -> Integer
    child :: a -> Integer

instance (Integral a,Integral b) => HierarchicalRelation (a,b) where
    parent = toInteger . snd
    child = toInteger . fst

-- | A tree or hierarchy based on records that represent parent-child relations.
data HierarchicalDatabase a =
    HierarchicalDatabase {
        hd_children :: (Map.Map Integer [Integer]),
        hd_parent :: (Map.Map Integer a) }

instance (Show a) => Show (HierarchicalDatabase a) where
    show imap = show $ toList imap

instance (HierarchicalRelation a,Read a) => Read (HierarchicalDatabase a) where
    readsPrec n v = Prelude.map foo (readsPrec n v)
        where foo (x,y) = (fromList x,y)

empty :: HierarchicalDatabase a
empty = HierarchicalDatabase (Map.empty) (Map.empty)

-- |
-- O(log n)  Inserts the specified (parent,child,user_data) pair into the
-- InsidessMap.  If the given child already has a parent, that parent is
-- replaced by the new one.
--
insert :: (HierarchicalRelation a) => a -> HierarchicalDatabase a -> HierarchicalDatabase a
insert a the_map =
    HierarchicalDatabase {
        hd_children = Map.alter (Just . maybe [child a] (child a :)) (parent a) $ 
                          hd_children $ delete (child a) the_map,
        hd_parent = Map.insert (child a) a $ hd_parent the_map }

-- |
-- Deletes the specified object from this insideness map.
--
delete :: (HierarchicalRelation a) => Integer -> HierarchicalDatabase a -> HierarchicalDatabase a
delete x the_map =
    HierarchicalDatabase {
        hd_children = maybe (hd_children the_map) (\p -> Map.update deleteChildFromList p $ hd_children the_map) xsParent,
        hd_parent = Map.delete x $ hd_parent the_map }
    where deleteChildFromList l = case List.delete x l of
                                       [] -> Nothing
                                       l' -> Just l'
          xsParent = parentOf x the_map

-- |
-- Answers the key of the parent of the given key, if any.
--
parentOf :: (HierarchicalRelation a) => Integer -> HierarchicalDatabase a -> Maybe Integer
parentOf x the_map = fmap parent $ Map.lookup x $ hd_parent the_map

-- |
-- Answers the parent relation and all children relations of a given key.
--
lookup :: (HierarchicalRelation a) => Integer -> HierarchicalDatabase a -> (Maybe a,[a])
lookup x the_map = (Map.lookup x $ hd_parent the_map,
                    maybe [] (Maybe.mapMaybe (flip Map.lookup (hd_parent the_map))) $ Map.lookup x $ hd_children the_map)

-- |
-- Answers the child relations of a given key.
--
lookupChildren :: (HierarchicalRelation a) => Integer -> HierarchicalDatabase a -> [a]
lookupChildren x the_map = snd $ lookup x the_map

-- |
-- Answers the parent relation of a given key, if any.
--
lookupParent :: (HierarchicalRelation a) => Integer -> HierarchicalDatabase a -> Maybe a
lookupParent x the_map = fst $ lookup x the_map

-- |
-- Answers the keys of the children for a given key.
--
childrenOf :: (HierarchicalRelation a) => Integer -> HierarchicalDatabase a -> [Integer]
childrenOf x the_map = maybe [] id $ Map.lookup x (hd_children the_map)
        
        
-- |
-- Converts a HierarchicalDatabase into a list of relations.
--
toList :: HierarchicalDatabase a -> [a]
toList the_map = List.map snd $ Map.toList $ hd_parent the_map

-- |
-- Converts a list of relations into a HierarchicalDatabase.
--
fromList :: (HierarchicalRelation a) => [a] -> HierarchicalDatabase a
fromList as = foldr (insert) empty as

data ExampleRelation = ExampleRelation (Integer,Integer,Bool)

instance HierarchicalRelation ExampleRelation where
    parent (ExampleRelation (n,_,_)) = n
    child (ExampleRelation (_,n,_)) = n

example1 :: HierarchicalDatabase ExampleRelation
example1 = fromList $ List.map ExampleRelation 
                                               [(1,13,True),
                                                (1,(-5),True),
                                                (1,1,True),
                                                (1,7,True),
                                                (1,15,True),
                                                (2,0,False),
                                                (3,12,True),
                                                (3,9,False),
                                                (3,(-3),True),
                                                (4,100,False),
                                                (4,(-6),False),
                                                (4,14,False)]

testParent :: HUnit.Test
testParent = HUnit.TestCase $ HUnit.assertEqual "testParent" (Just 2) (parentOf 0 example1)

testChildCount :: HUnit.Test
testChildCount = HUnit.TestCase $
    HUnit.assertEqual "testChildCount" 5 (length $ childrenOf 1 example1)

testUserData :: HUnit.Test
testUserData = let child_records = lookupChildren 1 example1
                   in HUnit.TestCase $ HUnit.assertBool "testUserData" (all (\(ExampleRelation (_,_,b)) -> b) child_records)

testChildrenEven :: HUnit.Test
testChildrenEven = let the_children = childrenOf 4 example1 -- The example data is contrived so that all of the children of "4" have even numbered IDs.
                        in HUnit.TestCase $ HUnit.assertBool "testChildrenEven" (all even the_children)

testDelete :: HUnit.Test
testDelete = let deleted = delete 0 $ delete (-6) $ example1
                 in HUnit.TestCase $ HUnit.assertBool "testDelete" ((length $ childrenOf 4 deleted) == 2 &&
                                                                    (isNothing $ parentOf 0 deleted))

testcases :: HUnit.Test
testcases = HUnit.TestLabel "HierarchicalDatabase" $ HUnit.TestList [testParent,testChildCount,testUserData,testChildrenEven,testDelete]
