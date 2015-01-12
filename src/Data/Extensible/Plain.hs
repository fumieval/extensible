{-# LANGUAGE DataKinds, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Plain
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
------------------------------------------------------------------------
module Data.Extensible.Plain (
  K0(..)
  , AllOf
  , OneOf
  , (<%)
  , pluck
  , bury
  , (<%|)
  , record
  , (<?%)
  )where
import Data.Extensible.Internal
import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Extensible.Match
import Data.Typeable
import Unsafe.Coerce
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Control.Applicative
-- | Just a value.
newtype K0 a = K0 { getK0 :: a } deriving (Eq, Ord, Read, Typeable, Functor, Foldable, Traversable)

instance Applicative K0 where
  pure = K0
  K0 f <*> K0 a = K0 (f a)

instance Monad K0 where
  return = K0
  K0 a >>= k = k a

instance Show a => Show (K0 a) where
  showsPrec d (K0 a) = showParen (d > 10) $ showString "K0 " . showsPrec 11 a

type AllOf xs = K0 :* xs
type OneOf xs = K0 :| xs

-- | /O(log n)/ Add a plain value to a product.
(<%) :: x -> AllOf xs -> AllOf (x ': xs)
(<%) = unsafeCoerce (<:*)
{-# INLINE (<%) #-}
infixr 5 <%

-- | Extract a plain value.
pluck :: (x ∈ xs) => AllOf xs -> x
pluck = getK0 . hlookup membership

-- | Embed a plain value.
bury :: (x ∈ xs) => x -> OneOf xs
bury = embed . K0

-- | Naive pattern matching for a plain value.
(<%|) :: (x -> r) -> (OneOf xs -> r) -> OneOf (x ': xs) -> r
(<%|) = unsafeCoerce (<:|)

-- | /O(log n)/ A lens for a plain value in a product.
record :: (x ∈ xs, Functor f) => (x -> f x) -> (AllOf xs -> f (AllOf xs))
record f = sector $ unsafeCoerce f `asTypeOf` (fmap K0 . f . getK0)
{-# INLINE record #-}

-- | Prepend a clause for a plain value.
(<?%) :: (x -> a) -> Match K0 a :* xs -> Match K0 a :* (x ': xs)
(<?%) = unsafeCoerce (<:*)
infixr 1 <?%

