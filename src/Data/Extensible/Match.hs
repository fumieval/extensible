{-# LANGUAGE PolyKinds #-}
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
  Match(..)
  , match
  , mapMatch
  , caseOf) where

import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Typeable

-- | Turn a wrapper type into one clause that returns @a@.
newtype Match h a x = Match { runMatch :: h x -> a } deriving Typeable

-- | Applies a function to the result of 'Match'.
mapMatch :: (a -> b) -> Match h a x -> Match h b x
mapMatch f (Match g) = Match (f . g)
{-# INLINE mapMatch #-}

-- | /O(log n)/ Perform pattern matching.
match :: Match h a :* xs -> h :| xs -> a
match p (UnionAt pos h) = runMatch (hlookup pos p) h
{-# INLINE match #-}

-- | Flipped `match`
caseOf :: h :| xs -> Match h a :* xs -> a
caseOf = flip match
{-# INLINE caseOf #-}
infix 0 `caseOf`
