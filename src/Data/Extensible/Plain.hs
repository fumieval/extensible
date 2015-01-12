{-# LANGUAGE DataKinds #-}
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
-- | Just a value.
newtype K0 a = K0 { getK0 :: a } deriving (Eq, Ord, Read, Typeable)

instance Show a => Show (K0 a) where
  showsPrec d (K0 a) = showParen (d > 10) $ showString "K0 " . showsPrec 11 a

-- | /O(log n)/ Add a plain value to a product.
(<%) :: x -> K0 :* xs -> K0 :* (x ': xs)
(<%) = unsafeCoerce (<:*)
{-# INLINE (<%) #-}
infixr 5 <%

-- | Extract a plain value.
pluck :: (x ∈ xs) => K0 :* xs -> x
pluck = getK0 . hlookup membership

-- | Embed a plain value.
bury :: (x ∈ xs) => x -> K0 :| xs
bury = embed . K0

-- | Naive pattern matching for a plain value.
(<%|) :: (x -> r) -> (K0 :| xs -> r) -> K0 :| (x ': xs) -> r
(<%|) = unsafeCoerce (<:|)

-- | /O(log n)/ A lens for a plain value in a product.
record :: (x ∈ xs, Functor f) => (x -> f x) -> (K0 :* xs -> f (K0 :* xs))
record f = sector $ unsafeCoerce f `asTypeOf` (fmap K0 . f . getK0)
{-# INLINE record #-}

-- | Prepend a clause for a plain value.
(<?%) :: (x -> a) -> Match K0 a :* xs -> Match K0 a :* (x ': xs)
(<?%) = unsafeCoerce (<:*)
infixr 1 <?%
