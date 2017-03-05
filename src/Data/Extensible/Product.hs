{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Product
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
------------------------------------------------------------------------
module Data.Extensible.Product (
  -- * Basic operations
  (:*)
  , nil
  , (<:)
  , hlength
  , hmap
  , hmapWithIndex
  , hzipWith
  , hzipWith3
  , hfoldMap
  , hfoldMapWithIndex
  , hfoldrWithIndex
  , htraverse
  , htraverseWithIndex
  , hsequence
  -- * Update
  , haccumMap
  , haccum
  , hpartition
  -- * Lookup
  , hlookup
  , hindex
  -- * Generation
  , Generate(..)
  , hgenerate
  , htabulate
  , hrepeat
  , hcollect
  , hdistribute
  , fromHList
  , toHList
  , Forall(..)
  , hgenerateFor
  , htabulateFor
  , hrepeatFor) where

import Data.Extensible.Internal
import Data.Extensible.Struct
import Data.Extensible.Sum
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Data.Extensible.Class
import qualified Data.Extensible.HList as HList
import Data.Extensible.Wrapper

-- | O(n) Prepend an element onto a product.
-- Expressions like @a <: b <: c <: nil@ are transformed to a single 'fromHList'.
(<:) :: h x -> h :* xs -> h :* (x ': xs)
(<:) x = fromHList . HList.HCons x . toHList
{-# INLINE (<:) #-}
infixr 0 <:

-- | An empty product.
nil :: h :* '[]
nil = hfrozen $ new $ error "Impossible"
{-# NOINLINE nil #-}
{-# RULES "toHList/nil" toHList nil = HList.HNil #-}

-- | Convert 'L.HList' into a product.
fromHList :: HList.HList h xs -> h :* xs
fromHList xs = hfrozen (newFromHList xs)
{-# NOINLINE fromHList #-}
{-# RULES "toHList/fromHList" forall x. toHList (fromHList x) = x #-}

-- | Flipped 'hlookup'
hindex :: h :* xs -> Membership xs x ->  h x
hindex = flip hlookup
{-# INLINE hindex #-}

-- | Map a function to every element of a product.
hmapWithIndex :: (forall x. Membership xs x -> g x -> h x) -> g :* xs -> h :* xs
hmapWithIndex t p = hfrozen (newFrom p t)
{-# INLINE hmapWithIndex #-}

-- | Transform every element in a product, preserving the order.
--
-- @
-- 'hmap' 'id' ≡ 'id'
-- 'hmap' (f . g) ≡ 'hmap' f . 'hmap' g
-- @
hmap :: (forall x. g x -> h x) -> g :* xs -> h :* xs
hmap f = hmapWithIndex (const f)
{-# INLINE hmap #-}

-- | 'zipWith' for heterogeneous product
hzipWith :: (forall x. f x -> g x -> h x) -> f :* xs -> g :* xs -> h :* xs
hzipWith t xs = hmapWithIndex (\i -> t (hlookup i xs))
{-# INLINE hzipWith #-}

-- | 'zipWith3' for heterogeneous product
hzipWith3 :: (forall x. f x -> g x -> h x -> i x) -> f :* xs -> g :* xs -> h :* xs -> i :* xs
hzipWith3 t xs ys = hmapWithIndex (\i -> t (hlookup i xs) (hlookup i ys))
{-# INLINE hzipWith3 #-}

-- | Map elements to a monoid and combine the results.
--
-- @'hfoldMap' f . 'hmap' g ≡ 'hfoldMap' (f . g)@
hfoldMap :: Monoid a => (forall x. h x -> a) -> h :* xs -> a
hfoldMap f = hfoldMapWithIndex (const f)
{-# INLINE hfoldMap #-}

-- | 'hfoldMap' with the membership of elements.
hfoldMapWithIndex :: Monoid a
  => (forall x. Membership xs x -> g x -> a) -> g :* xs -> a
hfoldMapWithIndex f = hfoldrWithIndex (\i -> mappend . f i) mempty
{-# INLINE hfoldMapWithIndex #-}

-- | Traverse all elements and combine the result sequentially.
-- @
-- htraverse (fmap f . g) ≡ fmap (hmap f) . htraverse g
-- htraverse pure ≡ pure
-- htraverse (Comp . fmap g . f) ≡ Comp . fmap (htraverse g) . htraverse f
-- @
htraverse :: Applicative f => (forall x. g x -> f (h x)) -> g :* xs -> f (h :* xs)
htraverse f = fmap fromHList . HList.htraverse f . toHList
{-# INLINE htraverse #-}

-- | 'sequence' analog for extensible products
hsequence :: Applicative f => Comp f h :* xs -> f (h :* xs)
hsequence = htraverse getComp
{-# INLINE hsequence #-}

-- | The dual of 'htraverse'
hcollect :: (Functor f, Generate xs) => (a -> h :* xs) -> f a -> Comp f h :* xs
hcollect f m = htabulate $ \i -> Comp $ fmap (hlookup i . f) m
{-# INLINABLE hcollect #-}

-- | The dual of 'hsequence'
hdistribute :: (Functor f, Generate xs) => f (h :* xs) -> Comp f h :* xs
hdistribute = hcollect id
{-# INLINE hdistribute #-}

-- | 'htraverse' with 'Membership's.
htraverseWithIndex :: Applicative f
  => (forall x. Membership xs x -> g x -> f (h x)) -> g :* xs -> f (h :* xs)
htraverseWithIndex f = fmap fromHList . HList.htraverseWithIndex f . toHList
{-# INLINE htraverseWithIndex #-}

-- | A product filled with the specified value.
hrepeat :: Generate xs => (forall x. h x) -> h :* xs
hrepeat x = hfrozen $ newRepeat x
{-# INLINE hrepeat #-}

-- | Construct a product using a function which takes a 'Membership'.
--
-- @
-- 'hmap' f ('htabulate' g) ≡ 'htabulate' (f . g)
-- 'htabulate' ('hindex' m) ≡ m
-- 'hindex' ('htabulate' k) ≡ k
-- @
htabulate :: Generate xs => (forall x. Membership xs x -> h x) -> h :* xs
htabulate f = hfrozen $ new f
{-# INLINE htabulate #-}

-- | 'Applicative' version of 'htabulate'.
hgenerate :: (Generate xs, Applicative f)
  => (forall x. Membership xs x -> f (h x)) -> f (h :* xs)
hgenerate f = fmap fromHList $ hgenerateList f
{-# INLINE hgenerate #-}

-- | Pure version of 'hgenerateFor'.
htabulateFor :: Forall c xs => proxy c -> (forall x. c x => Membership xs x -> h x) -> h :* xs
htabulateFor p f = hfrozen $ newFor p f
{-# INLINE htabulateFor #-}

-- | A product filled with the specified value.
hrepeatFor :: Forall c xs => proxy c -> (forall x. c x => h x) -> h :* xs
hrepeatFor p f = htabulateFor p (const f)
{-# INLINE hrepeatFor #-}

-- | 'Applicative' version of 'htabulateFor'.
hgenerateFor :: (Forall c xs, Applicative f)
  => proxy c -> (forall x. c x => Membership xs x -> f (h x)) -> f (h :* xs)
hgenerateFor p f = fmap fromHList $ hgenerateListFor p f
{-# INLINE hgenerateFor #-}

-- | Accumulate sums on a product.
haccumMap :: Foldable f
  => (a -> g :| xs)
  -> (forall x. Membership xs x -> g x -> h x -> h x)
  -> h :* xs -> f a -> h :* xs
haccumMap f g p0 xs = hfrozen $ do
  s <- thaw p0
  mapM_ (\x -> case f x of EmbedAt i v -> get s i >>= set s i . g i v) xs
  return s

-- | @haccum = 'haccumMap' 'id'@
haccum :: Foldable f
  => (forall x. Membership xs x -> g x -> h x -> h x)
  -> h :* xs -> f (g :| xs) -> h :* xs
haccum = haccumMap id
{-# INLINE haccum #-}

-- | Group sums by type.
hpartition :: (Foldable f, Generate xs) => (a -> h :| xs) -> f a -> Comp [] h :* xs
hpartition f = haccumMap f (\_ x (Comp xs) -> Comp (x:xs)) $ hrepeat $ Comp []
{-# INLINE hpartition #-}
