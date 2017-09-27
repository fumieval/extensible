{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Plain
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
------------------------------------------------------------------------
module Data.Extensible.Plain (
    AllOf
  , OneOf
  , (<%)
  , pluck
  , bury
  , (<%|)
  , accessing
  ) where
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Extensible.Class
import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Functor.Identity
import Data.Extensible.Wrapper
import Data.Coerce
import Data.Profunctor.Unsafe

-- | Alias for plain products
type AllOf xs = Identity :* xs

-- | Alias for plain sums
type OneOf xs = Identity :| xs

-- | /O(log n)/ Add a plain value to a product.
(<%) :: x -> AllOf xs -> AllOf (x ': xs)
(<%) = (<:) .# Identity
{-# INLINE (<%) #-}
infixr 5 <%

-- | Extract a plain value.
pluck :: (x ∈ xs) => AllOf xs -> x
pluck = views piece runIdentity
{-# INLINE pluck #-}

-- | Embed a plain value.
bury :: (x ∈ xs) => x -> OneOf xs
bury = embed .# Identity
{-# INLINE bury #-}

-- | Naive pattern matching for a plain value.
(<%|) :: (x -> r) -> (OneOf xs -> r) -> OneOf (x ': xs) -> r
(<%|) = (<:|) . (.# runIdentity)
infixr 1 <%|

-- | An accessor for newtype constructors.
accessing :: (Coercible x a, x ∈ xs, Extensible f p t, ExtensibleConstr t Identity xs x) => (a -> x) -> Optic' p f (t Identity xs) a
accessing c = piece . _Wrapper . dimap coerce (fmap c)
{-# INLINE accessing #-}
