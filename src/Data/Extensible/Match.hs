-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.League
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Pattern matching
------------------------------------------------------------------------
module Data.Extensible.Match (
  matchWith
  , Match(..)
  , _Match
  , match
  , mapMatch
  , caseOf) where

import Data.Extensible.Internal.Rig
import Data.Extensible.Class
import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Profunctor
import Data.Typeable

-- | Retrieve the contents so that they matches and pass both to the given function.
matchWith :: (forall x. f x -> g x -> r) -> f :* xs -> g :| xs -> r
matchWith f p = \(EmbedAt i h) -> views (pieceAt i) f p h
{-# INLINE matchWith #-}

-- | Applies a function to the result of 'Match'.
mapMatch :: (a -> b) -> Match h a x -> Match h b x
mapMatch f (Match g) = Match (f . g)
{-# INLINE mapMatch #-}

-- | /O(log n)/ Perform pattern matching.
match :: Match h a :* xs -> h :| xs -> a
match = matchWith runMatch
{-# INLINE match #-}

-- | Flipped `match`
caseOf :: h :| xs -> Match h a :* xs -> a
caseOf = flip match
{-# INLINE caseOf #-}
infix 0 `caseOf`

-- | Turn a wrapper type into one clause that returns @a@.
newtype Match h a x = Match { runMatch :: h x -> a } deriving Typeable

_Match :: (Profunctor p, Functor f) => p (g x -> a) (f (h y -> b)) -> p (Match g a x) (f (Match h b y))
_Match = dimap runMatch (fmap Match)
