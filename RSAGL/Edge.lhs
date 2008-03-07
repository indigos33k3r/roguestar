\section{Edge Detection in FRP programs: RSAGL.Edge}

Edge detection works for all inputs that implement Eq, and is simply a mechanism to observe when a value changes.

\begin{code}
{-# OPTIONS_GHC -farrows -fglasgow-exts #-}

module RSAGL.Edge 
    (Edge(..),
     edge,
     edgep,
     edgeFold,
     genericEdgeFold,
     edgeMap,
     genericEdgeMap,
     history,
     sticky,
     initial,
     started)
    where

import RSAGL.AbstractVector
import RSAGL.FRP as FRP
import RSAGL.SwitchedArrow as SwitchedArrow
import RSAGL.Time
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Data.Maybe
\end{code}

\subsection{Edge data structure}

\begin{code}
data Edge a = Edge { edge_previous :: !a,
                     edge_next :: !a,
                     edge_changed :: !Time }
              deriving (Eq,Show)
\end{code}

\subsection{Edge detectors}

\texttt{edge} watches an input and answers an Edge data structure.

\begin{code}
edge :: (Arrow a,ArrowChoice a,ArrowApply a,Eq e) => FRPX any t i o a e (Edge e)
edge = proc i ->
    do t <- threadTime -< ()
       FRP.statefulContext $ SwitchedArrow.withState edge' initial_edge -< (t,i)
    where initial_edge (t,i) = Edge { edge_previous = i, 
                                      edge_next = i, 
                                      edge_changed = t }
          edge' = proc (t,i) ->
              do e <- lift fetch -< ()
                 lift store -< if i /= edge_next e
                               then Edge { edge_previous = edge_next e,
                                           edge_next = i,
                                           edge_changed = t }
                               else e
                 lift fetch -< ()
\end{code}

\texttt{edgep} answers True exactly once each time a value changes.

\begin{code}
edgep :: (Arrow a,ArrowChoice a,ArrowApply a,Eq e) => FRPX any t i o a e Bool
edgep = FRP.statefulContext $ SwitchedArrow.withState edgep' id
    where edgep' = proc i ->
              do old_value <- lift fetch -< ()
                 lift store -< i
                 returnA -< i /= old_value
\end{code}

\subsection{Edge folds}

\texttt{edgeFold} combines each unique input into a cumulative value using a folding function.
The folding function must have some way to discard old edges, or it will represent a space leak.

\begin{code}
edgeFold :: (Arrow a,ArrowChoice a,ArrowApply a,Eq j) => p -> (j -> p -> p) -> FRPX any t i o a j p
edgeFold = genericEdgeFold (==)

genericEdgeFold :: (Arrow a,ArrowChoice a,ArrowApply a) => (j -> j -> Bool) -> p -> (j -> p -> p) -> FRPX any t i o a j p
genericEdgeFold predicate initial_value f = FRP.statefulContext $ SwitchedArrow.withState edgeFold' (\i -> (i,f i initial_value))
    where edgeFold' = proc i ->
              do (old_raw,old_folded) <- lift fetch -< ()
                 let (new_raw,new_folded) = if old_raw `predicate` i
                                            then (old_raw,old_folded)
                                            else (i,f i old_folded)
                 lift store -< seq new_raw $ seq new_folded $ (new_raw,new_folded)
                 returnA -< new_folded
\end{code}

\texttt{history} answers a history of edges for a value.  The Time parameter indicates the maximum age of an edge, 
after which it can be forgotten.

\begin{code}
history :: (Arrow a,ArrowChoice a,ArrowApply a,Eq e) => Time -> FRPX any t i o a e [Edge e]
history t = edgeFold [] history' <<< edge
    where history' n h = n : takeWhile ((>= edge_changed n `sub` t) . edge_changed) h
\end{code}

\texttt{edgeMap} is equivalent to \texttt{arr}, but more efficient, possibly, because the mapping function is only 
applied when the input changes.

\texttt{edgeMap} should actually be more efficient only when: the cost of the mapping function is high, the cost
of comparing two equal values is low, and the input changes infrequently.

\begin{code}
edgeMap :: (Arrow a,ArrowChoice a,ArrowApply a,Eq j) => (j -> p) -> FRPX any t i o a j p
edgeMap f = genericEdgeMap (==) f

genericEdgeMap :: (Arrow a,ArrowChoice a,ArrowApply a,Eq j) => (j -> j -> Bool) -> (j -> p) -> FRPX any t i o a j p
genericEdgeMap predicate f = genericEdgeFold predicate (error "genericEdgeMap: undefined initial value") (const . f)
\end{code}

\texttt{sticky} remembers the most recent value of an input that satisfies some criteria.

\begin{code}
sticky :: (Arrow a,ArrowChoice a,ArrowApply a) => (x -> Bool) -> x -> FRPX any t i o a x x
sticky criteria initial_value = genericEdgeFold (const $ const False) initial_value (\new old -> if criteria new then new else old)
\end{code}

\subsection{Remembering the initial value of an input}

\texttt{initial} answers the first value that an input ever has (during this instance of this thread).

\begin{code}
initial :: (Arrow a,ArrowChoice a,ArrowApply a,Eq e) => FRPX any t i o a e e
initial = FRP.statefulContext $ SwitchedArrow.withState initial1 (error "initial: undefined")
    where initial1 = proc i ->
              do lift store -< i
                 SwitchedArrow.switchContinue -< (Just $ initial2,i)
          initial2 = lift fetch
\end{code}

\texttt{started} is the \texttt{absoluteTime} at which this thread started.

\begin{code}
started :: (Arrow a,ArrowChoice a,ArrowApply a) => FRPX any t i o a () Time
started = initial <<< absoluteTime
\end{code}

