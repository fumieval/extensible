{-# LANGUAGE LambdaCase, TypeFamilies, DeriveGeneric, GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Nullable
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
------------------------------------------------------------------------
module Data.Extensible.Nullable (
  vacancy
  , coinclusion
  , wrench
  , retrench
  , Nullable(..)
  , mapNullable) where

import Control.DeepSeq (NFData)
import Data.Extensible.Class
import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Extensible.Inclusion
import Data.Extensible.Internal.Rig
import Data.Typeable (Typeable)
import Data.Extensible.Wrapper
import qualified Data.Extensible.Struct as S
import Data.Profunctor.Unsafe
import Data.Semigroup
import GHC.Generics (Generic)

-- | Wrapped Maybe
newtype Nullable h x = Nullable { getNullable :: Maybe (h x) }
  deriving (Show, Eq, Ord, Typeable, Generic, NFData)

instance Wrapper h => Wrapper (Nullable h) where
  type Repr (Nullable h) x = Maybe (Repr h x)
  _Wrapper = withIso _Wrapper $ \f g -> dimap (fmap f . getNullable) (fmap (Nullable . fmap g))

instance Semigroup (h x) => Monoid (Nullable h x) where
  mempty = Nullable Nothing
  mappend (Nullable (Just a)) (Nullable (Just b)) = Nullable (Just (a <> b))
  mappend a@(Nullable (Just _)) _ = a
  mappend _ b = b

-- | Apply a function to its content.
mapNullable :: (g x -> h y) -> Nullable g x -> Nullable h y
mapNullable f = Nullable #. fmap f .# getNullable
{-# INLINE mapNullable #-}

-- | The inverse of 'inclusion'.
coinclusion :: (Include ys xs, Generate ys) => Nullable (Membership xs) :* ys
coinclusion = S.hfrozen $ do
  s <- S.newRepeat $ Nullable Nothing
  hfoldrWithIndex
    (\i m cont -> S.set s m (Nullable $ Just i) >> cont) (return s) inclusion

-- | A product filled with @'Nullable' 'Nothing'@
vacancy :: Generate xs => Nullable h :* xs
vacancy = hrepeat $ Nullable Nothing

-- | Extend a product and fill missing fields by 'Null'.
wrench :: (Generate ys, xs ⊆ ys) => h :* xs -> Nullable h :* ys
wrench xs = mapNullable (flip hlookup xs) `hmap` coinclusion
{-# INLINE wrench #-}

-- | Narrow the range of the sum, if possible.
retrench :: (Generate ys, xs ⊆ ys) => h :| ys -> Nullable ((:|) h) xs
retrench (EmbedAt i h) = views (pieceAt i) (mapNullable (`EmbedAt`h)) coinclusion
{-# INLINE retrench #-}
