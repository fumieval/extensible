{-# LANGUAGE DeriveDataTypeable #-}
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
-- Polymorphic open unions
------------------------------------------------------------------------
module Data.Extensible.Union where

import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Extensible.Sum
import Data.Extensible.Product
import Data.Typeable

-- | Wrap a type that has a kind @* -> *@.
newtype K1 a f = K1 { getK1 :: f a } deriving (Eq, Ord, Read, Typeable)

newtype Union xs a = Union { getUnion :: K1 a :| xs }

reunion :: Gondola m :* xs -> Union xs a -> m a
reunion gs (Union (UnionAt pos (K1 f))) = views (sectorAt pos) runGondola gs f

-- | Transformation between effects
newtype Gondola f g = Gondola { runGondola :: forall a. g a -> f a }

-- | Add a new transformation.
rung :: (forall x. f x -> g x) -> Gondola g :* fs -> Gondola g :* (f ': fs)
rung f = (<:) (Gondola f)
infixr 0 `rung`

runGondolas :: (x ∈ xs) => Gondola f :* xs -> x a -> f a
runGondolas = views sector runGondola
{-# INLINE runGondolas #-}
