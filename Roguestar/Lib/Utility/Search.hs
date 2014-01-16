{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
module Roguestar.Lib.Utility.Search
    (Searchable(..),
     Graph, Cost, Priority, Acceptance, Path, Paths,
     searchableChessboard,
     floodfillChessboard,
     startingAt,
     search)
    where

import qualified Data.Map as Map
import Data.List as List

data Searchable a = Searchable {
    searchable_graph :: Graph a,
    searchable_cost  :: Cost a,
    searchable_heuristic :: Priority a,
    searchable_acceptance :: Acceptance a }

chessboard :: Searchable (Integer,Integer)
chessboard = Searchable {
    searchable_graph = \(x,y) -> [(x-1,y-1), (x,y-1), (x+1,y-1),
                                  (x-1,y)  ,          (x+1,y),
                                  (x-1,y+1), (x,y+1), (x+1,y+1)],
    searchable_cost = \(x1,y1) (x2,y2) -> max (abs (x1-x2)) (abs (y1-y2)),
    searchable_heuristic = error "chessboard: undefined searchable_heuristic",
    searchable_acceptance = error "chessboard: undefined searchable_acceptance" }

searchableChessboard :: (Integer,Integer) -> Searchable (Integer,Integer)
searchableChessboard (goal_x,goal_y) = chessboard {
    searchable_heuristic = \(cost,(x,y):_) -> cost + max (abs (goal_x-x)) (abs (goal_y-y)),
    searchable_acceptance = \m -> Map.member (goal_x,goal_y) m }

floodfillChessboard :: ((Integer,Integer) -> Bool) -> Searchable (Integer,Integer)
floodfillChessboard f = chessboard {
    searchable_graph = filter f . (searchable_graph chessboard),
    searchable_heuristic = \(cost,_) -> cost,
    searchable_acceptance = const False }

-- |
-- Specifies a Graph by providing the set of nodes connected to any given node.
--
type Graph a = a -> [a]

-- |
-- Specifies the cost of moving from one node to another.
--
type Cost a = a -> a -> Integer

-- |
-- Specifies the priority with which nodes should be expanded during the search.
-- Lower means higher priority.
-- The parameter is the path to the node and the cost of that path.
--
type Priority a = Path a -> Integer

-- |
-- Specifies whether or not the listed set satisfies the goal of the search.
-- The search will end when this function evaluates to True.
--
type Acceptance a = Paths a -> Bool

-- |
-- Specifies a path to a specific node, paired with its cost.
-- The head of the list is the node itself, tracing back to the origin.
--
type Path a = (Integer,[a])
type Paths a = Map.Map a (Path a)

data Queue a = Queue {
    search_queue :: [Path a],
    best_paths :: Paths a }

startingAt :: (Ord a) => a -> Queue a
startingAt a = Queue [initial_path] (Map.singleton a initial_path)
    where initial_path = (0,[a])

search :: forall a. (Ord a) => Searchable a -> Queue a -> Paths a
search _          queue | null (search_queue queue) = best_paths queue
search searchable queue | searchable_acceptance searchable (best_paths queue) = best_paths queue
search searchable queue = search searchable $ Queue new_search_queue new_paths
    where path_to_here :: Path a
          path_to_here@(_,here:_) = head $ search_queue queue
          paths_from_here :: [Path a]
          paths_from_here = filter (\x -> isImprovement x (best_paths queue)) $ map (expand searchable path_to_here) $ searchable_graph searchable here
          new_search_queue :: [Path a]
          new_search_queue = foldr List.insert (tail $ search_queue queue) paths_from_here
          new_paths :: Paths a
          new_paths = foldr addPath (best_paths queue) paths_from_here

expand :: Searchable a -> Path a -> a -> Path a
expand searchable (cost_so_far,path_so_far) a =
    (cost_so_far + searchable_cost searchable a (head path_so_far), a:path_so_far)

isImprovement :: (Ord a) => Path a -> Paths a -> Bool
isImprovement (new_cost,new_steps) paths | (Just (old_cost,_)) <- Map.lookup (head new_steps) paths = old_cost > new_cost
isImprovement _ _ | otherwise = True

addPath :: (Ord a) => Path a -> Paths a -> Paths a
addPath new@(_,new_steps) paths | isImprovement new paths = Map.insert (head new_steps) new paths
addPath _                 paths | otherwise               = paths
