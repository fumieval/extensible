{-# LANGUAGE LambdaCase, TypeFamilies, TemplateHaskell #-}
------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Nullable
-- Copyright   :  (c) Fumiaki Kinoshita 2018
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
  , mapNullable
  , fromNullable
  , coinclusionAssoc
  , wrenchAssoc
  , retrenchAssoc) where

import Control.DeepSeq (NFData)
import Data.Extensible.Class
import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Extensible.Inclusion
import Data.Extensible.Internal.Rig
import Data.Hashable
import Data.Typeable (Typeable)
import Data.Extensible.Wrapper
import qualified Data.Extensible.Struct as S
import Data.Profunctor.Unsafe
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Language.Haskell.TH.Lift
import Test.QuickCheck.Arbitrary

-- | Wrapped Maybe
newtype Nullable h x = Nullable { getNullable :: Maybe (h x) }
  deriving (Show, Eq, Ord, Typeable, Generic, NFData, Arbitrary, Hashable, Lift)

instance Wrapper h => Wrapper (Nullable h) where
  type Repr (Nullable h) x = Maybe (Repr h x)
  _Wrapper = withIso _Wrapper $ \f g -> dimap (fmap f . getNullable) (fmap (Nullable . fmap g))

instance Semigroup (h x) => Semigroup (Nullable h x) where
  Nullable (Just a) <> Nullable (Just b) = Nullable (Just (a <> b))
  a@(Nullable (Just _)) <> _ = a
  _ <> b = b

instance Semigroup (h x) => Monoid (Nullable h x) where
  mempty = Nullable Nothing
  mappend = (<>)

-- | Apply a function to its content.
mapNullable :: (g x -> h y) -> Nullable g x -> Nullable h y
mapNullable f = Nullable #. fmap f .# getNullable
{-# INLINE mapNullable #-}

-- | The inverse of 'inclusion'.
coinclusion :: (Include ys xs, Generate ys) => ys :& Nullable (Membership xs)
coinclusion = S.hfrozen $ do
  s <- S.newRepeat $ Nullable Nothing
  hfoldrWithIndex
    (\i m cont -> S.set s m (Nullable $ Just i) >> cont) (return s) inclusion

-- | A product filled with @'Nullable' 'Nothing'@
vacancy :: Generate xs => xs :& Nullable h
vacancy = hrepeat $ Nullable Nothing

-- | Extend a product and fill missing fields by 'Null'.
wrench :: (Generate ys, xs ⊆ ys) => xs :& h -> ys :& Nullable h
wrench xs = mapNullable (flip hlookup xs) `hmap` coinclusion
{-# INLINE wrench #-}

-- | Narrow the range of the sum, if possible.
retrench :: (Generate ys, xs ⊆ ys) => ys :/ h -> Nullable ((:/) xs) h
retrench (EmbedAt i h) = views (pieceAt i) (mapNullable (`EmbedAt`h)) coinclusion
{-# INLINE retrench #-}

-- | 'fromMaybe' for 'Nullable'.
fromNullable :: h x -> Nullable h x -> h x
fromNullable def = fromMaybe def . getNullable
{-# INLINE fromNullable #-}

------------------------------------------------------------------

-- | The inverse of 'inclusionAssoc'.
coinclusionAssoc :: (IncludeAssoc ys xs, Generate ys) => ys :& Nullable (Membership xs)
coinclusionAssoc = S.hfrozen $ do
  s <- S.newRepeat $ Nullable Nothing
  hfoldrWithIndex
    (\i m cont -> S.set s m (Nullable $ Just i) >> cont) (return s) inclusionAssoc

-- | Extend a product and fill missing fields by 'Null'.
wrenchAssoc :: (Generate ys, IncludeAssoc ys xs) => xs :& h -> ys :& Nullable h
wrenchAssoc xs = mapNullable (flip hlookup xs) `hmap` coinclusionAssoc
{-# INLINE wrenchAssoc #-}

-- | Narrow the range of the sum, if possible.
retrenchAssoc :: (Generate ys, IncludeAssoc ys xs) => ys :/ h -> Nullable ((:/) xs) h
retrenchAssoc (EmbedAt i h) = views (pieceAt i) (mapNullable (`EmbedAt`h)) coinclusionAssoc
{-# INLINE retrenchAssoc #-}
