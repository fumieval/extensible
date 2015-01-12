{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Union
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
------------------------------------------------------------------------
module Data.Extensible.Union (K1(..)
  , (<?!)
  , Union(..)
  , liftU
  ) where

import Data.Typeable
import Data.Extensible.Internal
import Data.Extensible.Sum
import Data.Extensible.Product
import Data.Extensible.Match
import Unsafe.Coerce

-- | Wrap a type that has a kind @* -> *@.
newtype K1 a f = K1 { getK1 :: f a } deriving (Eq, Ord, Read, Typeable)

instance Show (f a) => Show (K1 a f) where
  showsPrec d (K1 a) = showParen (d > 10) $ showString "K1 " . showsPrec 11 a

-- | Prepend a clause for a parameterized value.
(<?!) :: (f x -> a) -> Match (K1 x) a :* xs -> Match (K1 x) a :* (f ': fs)
(<?!) = unsafeCoerce (<:*)
infixr 1 <?!

-- | A wrapper for @'K1' a ':|'' fs@ for having a kind @* -> *@.
newtype Union fs a = Union { getUnion :: K1 a :| fs } deriving Typeable

-- | /O(log n)/ Lift a value.
liftU :: (f ∈ fs) => f a -> Union fs a
liftU = Union . embed . K1
{-# INLINE liftU #-}

instance Show (Union '[] a) where
  show (Union u) = exhaust u

instance (Show (f a), Show (Union fs a)) => Show (Union (f ': fs) a) where
  showsPrec d (Union u) = (\(K1 f) -> showParen (d > 10) $ showString "liftU " . showsPrec 11 f)
    <:| showsPrec d . Union
    $ u

instance Functor (Union '[]) where
  fmap _ = exhaust . getUnion

-- | slow fmap
instance (Functor f, Functor (Union fs)) => Functor (Union (f ': fs)) where
  fmap f (Union (UnionAt pos (K1 h))) = case runPosition pos of
    Left Refl -> Union $ UnionAt pos $ K1 (fmap f h)
    Right pos' -> case fmap f (Union (UnionAt pos' (K1 h))) of
      Union (UnionAt _ h') -> Union (UnionAt (unsafeCoerce pos) h')
