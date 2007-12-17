\section{Applicative Wrapper}

\texttt{ApplicativeWrapper} is a simple wrapper over the Applicative typeclass that retains information about the purity of the wrapper data structure type.

\begin{code}
{-# OPTIONS_GHC -fglasgow-exts #-}

module RSAGL.ApplicativeWrapper
    (ApplicativeWrapper(..),
     fromPure,
     toApplicative,
     unwrapApplicative,
     wrapApplicative,
     isPure)
    where

import Control.Applicative
import Data.Maybe
import Control.Parallel.Strategies

newtype ApplicativeWrapper f a = ApplicativeWrapper (Either (f a) a)

instance (Functor f,Applicative f) => Functor (ApplicativeWrapper f) where
    fmap f (ApplicativeWrapper (Right a)) = pure $ f a
    fmap f (ApplicativeWrapper (Left a)) = wrapApplicative $ fmap f a

instance (Applicative f) => Applicative (ApplicativeWrapper f) where
    pure = ApplicativeWrapper . Right
    (ApplicativeWrapper (Right f)) <*> (ApplicativeWrapper (Right a)) = ApplicativeWrapper $ Right $ f a
    f <*> a = wrapApplicative $ toApplicative f <*> toApplicative a

instance (NFData (f a),NFData a) => NFData (ApplicativeWrapper f a) where
    rnf (ApplicativeWrapper ethr) = either rnf rnf ethr

fromPure :: (Applicative f) => ApplicativeWrapper f a -> Maybe a
fromPure = either (const Nothing) Just . unwrapApplicative

toApplicative :: (Applicative f) => ApplicativeWrapper f a -> f a
toApplicative = either id pure . unwrapApplicative

unwrapApplicative :: (Applicative f) => ApplicativeWrapper f a -> Either (f a) a
unwrapApplicative (ApplicativeWrapper x) = x

wrapApplicative :: (Applicative f) => f a -> ApplicativeWrapper f a
wrapApplicative = ApplicativeWrapper . Left

isPure :: (Applicative f) => ApplicativeWrapper f a -> Bool
isPure = isJust . fromPure
\end{code}