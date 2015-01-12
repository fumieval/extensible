{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Sum
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
------------------------------------------------------------------------
module Data.Extensible.Sum (
   (:|)(..)
  , hoist
  , embed
  , (<:|)
  , exhaust
  , picked
  ) where

import Data.Extensible.Internal
import Data.Type.Equality
import Control.Applicative
import Data.Typeable

-- | The extensible sum type
data (h :: k -> *) :| (s :: [k]) where
  UnionAt :: Position xs x -> h x -> h :| xs
deriving instance Typeable (:|)

-- | Change the wrapper.
hoist :: (forall x. g x -> h x) -> g :| xs -> h :| xs
hoist f (UnionAt pos h) = UnionAt pos (f h)
{-# INLINE hoist #-}

-- | /O(log n)/ lift a value.
embed :: (x ∈ xs) => h x -> h :| xs
embed = UnionAt membership
{-# INLINE embed #-}

instance Show (h :| '[]) where
  show = exhaust

instance (Show (h x), Show (h :| xs)) => Show (h :| (x ': xs)) where
  showsPrec d = (\h -> showParen (d > 10) $ showString "inS " . showsPrec 11 h)
    <:| showsPrec d

-- | /O(1)/ Naive pattern match
(<:|) :: (h x -> r) -> (h :| xs -> r) -> h :| (x ': xs) -> r
(<:|) r c = \(UnionAt pos h) -> case runPosition pos of
  Left Refl -> r h
  Right pos' -> c (UnionAt pos' h)
infixr 1 <:|
{-# INLINE (<:|) #-}

-- | There is no empty union.
exhaust :: h :| '[] -> r
exhaust _ = error "Impossible"

-- | A traversal that tries to point a specific element.
picked :: forall f h x xs. (x ∈ xs, Applicative f) => (h x -> f (h x)) -> h :| xs -> f (h :| xs)
picked f u@(UnionAt pos h) = case comparePosition (membership :: Position xs x) pos of
  Just Refl -> fmap (UnionAt pos) (f h)
  Nothing -> pure u
