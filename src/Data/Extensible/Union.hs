{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
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
module Data.Extensible.Union (K1(..), Union(..), Gondola(..), reunion, rung, runGondolas) where

import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Extensible.Class
import Data.Extensible.Sum
import Data.Extensible.Product
import Data.Extensible.Wrapper
import Data.Profunctor
import Data.Typeable (Typeable)

-- | Wrap a type that has a kind @* -> *@.
newtype K1 a f = K1 { getK1 :: f a } deriving (Eq, Ord, Read, Typeable)

instance Wrapper (K1 a) where
  type Repr (K1 a) f = f a
  _Wrapper = dimap getK1 (fmap K1)
  {-# INLINE _Wrapper #-}

newtype Union xs a = Union { getUnion :: K1 a :| xs }

reunion :: Gondola m :* xs -> Union xs a -> m a
reunion gs = \(Union (EmbedAt i (K1 f))) -> views (pieceAt i) runGondola gs f
{-# INLINE reunion #-}

-- | Transformation between effects
newtype Gondola f g = Gondola { runGondola :: forall a. g a -> f a }

-- | Add a new transformation.
rung :: (forall x. f x -> g x) -> Gondola g :* fs -> Gondola g :* (f ': fs)
rung f = (<:) (Gondola f)
{-# INLINE rung #-}

infixr 0 `rung`

runGondolas :: (x ∈ xs) => Gondola f :* xs -> x a -> f a
runGondolas = views piece runGondola
{-# INLINE runGondolas #-}
