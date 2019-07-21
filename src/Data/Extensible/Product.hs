{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Product
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
------------------------------------------------------------------------
module Data.Extensible.Product (
  -- * Basic operations
  (:&)
  , (:*)
  , nil
  , (<:)
  , (<!)
  , (=<:)
  , hlength
  , type (++)
  , happend
  , hmap
  , hmapWithIndex
  , hzipWith
  , hzipWith3
  , hfoldMap
  , hfoldMapWithIndex
  , hfoldrWithIndex
  , hfoldlWithIndex
  , htraverse
  , htraverseWithIndex
  , hsequence
  -- * Constrained fold
  , hmapWithIndexFor
  , hfoldMapFor
  , hfoldMapWithIndexFor
  , hfoldrWithIndexFor
  , hfoldlWithIndexFor
  -- * Constraind fold without proxies
  , hfoldMapWith
  , hfoldMapWithIndexWith
  , hfoldrWithIndexWith
  , hfoldlWithIndexWith
  , hmapWithIndexWith
  -- * Evaluating
  , hforce
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
  , hrepeatFor
  , hgenerateWith
  , htabulateWith
  , hrepeatWith) where

import Data.Extensible.Internal.Rig (review)
import Data.Extensible.Struct
import Data.Extensible.Sum
import Data.Extensible.Class
import Data.Extensible.Wrapper
import Data.Proxy
import qualified Type.Membership.HList as HList

-- | O(n) Prepend an element onto a product.
-- Expressions like @a <: b <: c <: nil@ are transformed to a single 'fromHList'.
(<:) :: h x -> xs :& h -> (x ': xs) :& h
(<:) x = fromHList . HList.HCons x . toHList
{-# INLINE (<:) #-}
infixr 0 <:

(=<:) :: Wrapper h => Repr h x -> xs :& h -> (x ': xs) :& h
(=<:) = (<:) . review _Wrapper
{-# INLINE (=<:) #-}
infixr 0 =<:

-- | Strict version of ('<:').
(<!) :: h x -> xs :& h -> (x ': xs) :& h
(<!) x = fromHList . (HList.HCons $! x) . toHList
{-# INLINE (<!) #-}
infixr 0 <!

-- | An empty product.
nil :: '[] :& h
nil = hfrozen $ new $ error "Impossible"
{-# NOINLINE nil #-}
{-# RULES "toHList/nil" toHList nil = HList.HNil #-}

-- | Convert 'HList.HList' into a product.
fromHList :: HList.HList h xs -> xs :& h
fromHList xs = hfrozen (newFromHList xs)
{-# INLINE fromHList #-}

-- | Flipped 'hlookup'
hindex :: xs :& h -> Membership xs x ->  h x
hindex = flip hlookup
{-# INLINE hindex #-}

-- | Map a function to every element of a product.
hmapWithIndex :: (forall x. Membership xs x -> g x -> h x) -> xs :& g -> xs :& h
hmapWithIndex t p = hfrozen (newFrom p t)
{-# INLINE hmapWithIndex #-}

-- | Map a function to every element of a product.
hmapWithIndexFor :: Forall c xs
  => proxy c
  -> (forall x. c x => Membership xs x -> g x -> h x)
  -> xs :& g -> xs :& h
hmapWithIndexFor c t p = hfrozen $ newFor c $ \i -> t i $ hlookup i p
{-# INLINE hmapWithIndexFor #-}

hmapWithIndexWith :: forall c xs g h. Forall c xs
  => (forall x. c x => Membership xs x -> g x -> h x)
  -> xs :& g -> xs :& h
hmapWithIndexWith = hmapWithIndexFor (Proxy @ c)

-- | Transform every element in a product, preserving the order.
--
-- @
-- 'hmap' 'id' ≡ 'id'
-- 'hmap' (f . g) ≡ 'hmap' f . 'hmap' g
-- @
hmap :: (forall x. g x -> h x) -> xs :& g -> xs :& h
hmap f = hmapWithIndex (const f)
{-# INLINE hmap #-}

-- | 'zipWith' for heterogeneous product
hzipWith :: (forall x. f x -> g x -> h x) -> xs :& f -> xs :& g -> xs :& h
hzipWith t xs = hmapWithIndex (\i -> t (hlookup i xs))
{-# INLINE hzipWith #-}

-- | 'zipWith3' for heterogeneous product
hzipWith3 :: (forall x. f x -> g x -> h x -> i x) -> xs :& f -> xs :& g -> xs :& h -> xs :& i
hzipWith3 t xs ys = hmapWithIndex (\i -> t (hlookup i xs) (hlookup i ys))
{-# INLINE hzipWith3 #-}

-- | Map elements to a monoid and combine the results.
--
-- @'hfoldMap' f . 'hmap' g ≡ 'hfoldMap' (f . g)@
hfoldMap :: Monoid a => (forall x. h x -> a) -> xs :& h -> a
hfoldMap f = hfoldMapWithIndex (const f)
{-# INLINE hfoldMap #-}

-- | 'hfoldMap' with the membership of elements.
hfoldMapWithIndex :: Monoid a
  => (forall x. Membership xs x -> g x -> a) -> xs :& g -> a
hfoldMapWithIndex f = hfoldrWithIndex (\i -> mappend . f i) mempty
{-# INLINE hfoldMapWithIndex #-}

-- | Perform a strict left fold over the elements.
hfoldlWithIndex :: (forall x. Membership xs x -> r -> h x -> r) -> r -> xs :& h -> r
hfoldlWithIndex f r xs = hfoldrWithIndex (\i x c a -> c $! f i a x) id xs r
{-# INLINE hfoldlWithIndex #-}

-- | 'hfoldrWithIndex' with a constraint for each element.
hfoldrWithIndexFor :: forall c xs h r proxy. (Forall c xs) => proxy c
  -> (forall x. c x => Membership xs x -> h x -> r -> r) -> r -> xs :& h -> r
hfoldrWithIndexFor p f r xs = henumerateFor p (Proxy :: Proxy xs) (\i -> f i (hlookup i xs)) r
{-# INLINE hfoldrWithIndexFor #-}

hfoldrWithIndexWith :: forall c xs h r. (Forall c xs)
  => (forall x. c x => Membership xs x -> h x -> r -> r) -> r -> xs :& h -> r
hfoldrWithIndexWith f r xs = henumerateFor (Proxy @ c) (Proxy @ xs) (\i -> f i (hlookup i xs)) r
{-# INLINE hfoldrWithIndexWith #-}

-- | Constrained 'hfoldlWithIndex'
hfoldlWithIndexFor :: (Forall c xs) => proxy c
  -> (forall x. c x => Membership xs x -> r -> h x -> r) -> r -> xs :& h -> r
hfoldlWithIndexFor p f r xs = hfoldrWithIndexFor p (\i x c a -> c $! f i a x) id xs r
{-# INLINE hfoldlWithIndexFor #-}

-- | Constrained 'hfoldlWithIndex'
hfoldlWithIndexWith :: forall c xs h r. (Forall c xs)
  => (forall x. c x => Membership xs x -> r -> h x -> r) -> r -> xs :& h -> r
hfoldlWithIndexWith f r xs = hfoldrWithIndexWith @c (\i x c a -> c $! f i a x) id xs r
{-# INLINE hfoldlWithIndexWith #-}

-- | 'hfoldMapWithIndex' with a constraint for each element.
hfoldMapWithIndexFor :: (Forall c xs, Monoid a) => proxy c
  -> (forall x. c x => Membership xs x -> h x -> a) -> xs :& h -> a
hfoldMapWithIndexFor p f = hfoldrWithIndexFor p (\i -> mappend . f i) mempty
{-# INLINE hfoldMapWithIndexFor #-}

-- | 'hfoldMapWithIndex' with a constraint for each element.
hfoldMapWithIndexWith :: forall c xs h a. (Forall c xs, Monoid a)
  => (forall x. c x => Membership xs x -> h x -> a) -> xs :& h -> a
hfoldMapWithIndexWith f = hfoldrWithIndexWith @c (\i -> mappend . f i) mempty
{-# INLINE hfoldMapWithIndexWith #-}

-- | Constrained 'hfoldMap'
hfoldMapFor :: (Forall c xs, Monoid a) => proxy c
  -> (forall x. c x => h x -> a) -> xs :& h -> a
hfoldMapFor p f = hfoldMapWithIndexFor p (const f)
{-# INLINE hfoldMapFor #-}

-- | Constrained 'hfoldMap'
hfoldMapWith :: forall c xs h a. (Forall c xs, Monoid a)
  => (forall x. c x => h x -> a) -> xs :& h -> a
hfoldMapWith f = hfoldMapWithIndexFor (Proxy @ c) (const f)
{-# INLINE hfoldMapWith #-}

-- | Traverse all elements and combine the result sequentially.
-- @
-- htraverse (fmap f . g) ≡ fmap (hmap f) . htraverse g
-- htraverse pure ≡ pure
-- htraverse (Comp . fmap g . f) ≡ Comp . fmap (htraverse g) . htraverse f
-- @
htraverse :: Applicative f => (forall x. g x -> f (h x)) -> xs :& g -> f (xs :& h)
htraverse f = fmap fromHList . HList.htraverse f . toHList
{-# INLINE htraverse #-}

-- | 'sequence' analog for extensible products
hsequence :: Applicative f => xs :& Comp f h -> f (xs :& h)
hsequence = htraverse getComp
{-# INLINE hsequence #-}

-- | The dual of 'htraverse'
hcollect :: (Functor f, Generate xs) => (a -> xs :& h) -> f a -> xs :& Comp f h
hcollect f m = htabulate $ \i -> Comp $ fmap (hlookup i . f) m
{-# INLINABLE hcollect #-}

-- | The dual of 'hsequence'
hdistribute :: (Functor f, Generate xs) => f (xs :& h) -> xs :& Comp f h
hdistribute = hcollect id
{-# INLINE hdistribute #-}

-- | 'htraverse' with 'Membership's.
htraverseWithIndex :: Applicative f
  => (forall x. Membership xs x -> g x -> f (h x)) -> xs :& g -> f (xs :& h)
htraverseWithIndex f = fmap fromHList . HList.htraverseWithIndex f . toHList
{-# INLINE htraverseWithIndex #-}

-- | A product filled with the specified value.
hrepeat :: Generate xs => (forall x. h x) -> xs :& h
hrepeat x = hfrozen $ newRepeat x
{-# INLINE hrepeat #-}

-- | Construct a product using a function which takes a 'Membership'.
--
-- @
-- 'hmap' f ('htabulate' g) ≡ 'htabulate' (f . g)
-- 'htabulate' ('hindex' m) ≡ m
-- 'hindex' ('htabulate' k) ≡ k
-- @
htabulate :: Generate xs => (forall x. Membership xs x -> h x) -> xs :& h
htabulate f = hfrozen $ new f
{-# INLINE htabulate #-}

-- | 'Applicative' version of 'htabulate'.
hgenerate :: (Generate xs, Applicative f)
  => (forall x. Membership xs x -> f (h x)) -> f (xs :& h)
hgenerate f = fmap fromHList $ hgenerateList f
{-# INLINE hgenerate #-}

-- | Pure version of 'hgenerateFor'.
htabulateFor :: Forall c xs => proxy c -> (forall x. c x => Membership xs x -> h x) -> xs :& h
htabulateFor p f = hfrozen $ newFor p f
{-# INLINE htabulateFor #-}

-- | Pure version of 'hgenerateFor'.
htabulateWith :: forall c xs h. Forall c xs => (forall x. c x => Membership xs x -> h x) -> xs :& h
htabulateWith f = hfrozen $ newFor (Proxy @ c) f
{-# INLINE htabulateWith #-}

-- | A product filled with the specified value.
hrepeatFor :: Forall c xs => proxy c -> (forall x. c x => h x) -> xs :& h
hrepeatFor p f = htabulateFor p (const f)
{-# INLINE hrepeatFor #-}

-- | A product filled with the specified value.
hrepeatWith :: forall c xs h. Forall c xs => (forall x. c x => h x) -> xs :& h
hrepeatWith f = htabulateFor (Proxy @ c) (const f)
{-# INLINE hrepeatWith #-}

-- | 'Applicative' version of 'htabulateFor'.
hgenerateFor :: (Forall c xs, Applicative f)
  => proxy c -> (forall x. c x => Membership xs x -> f (h x)) -> f (xs :& h)
hgenerateFor p f = fmap fromHList $ hgenerateListFor p f
{-# INLINE hgenerateFor #-}

-- | 'Applicative' version of 'htabulateFor'.
hgenerateWith :: forall c xs f h. (Forall c xs, Applicative f)
  => (forall x. c x => Membership xs x -> f (h x)) -> f (xs :& h)
hgenerateWith f = fmap fromHList $ hgenerateListFor (Proxy @ c) f
{-# INLINE hgenerateWith #-}

-- | Accumulate sums on a product.
haccumMap :: Foldable f
  => (a -> xs :/ g)
  -> (forall x. Membership xs x -> g x -> h x -> h x)
  -> xs :& h -> f a -> xs :& h
haccumMap f g p0 xs = hmodify
  (\s -> mapM_ (\x -> case f x of EmbedAt i v -> get s i >>= set s i . g i v) xs)
  p0
{-# INLINE haccumMap #-}

-- | @haccum = 'haccumMap' 'id'@
haccum :: Foldable f
  => (forall x. Membership xs x -> g x -> h x -> h x)
  -> xs :& h -> f (xs :/ g) -> xs :& h
haccum = haccumMap id
{-# INLINE haccum #-}

-- | Group sums by type.
hpartition :: (Foldable f, Generate xs) => (a -> xs :/ h) -> f a -> xs :& Comp [] h
hpartition f = haccumMap f (\_ x (Comp xs) -> Comp (x:xs)) $ hrepeat $ Comp []
{-# INLINE hpartition #-}

-- | Evaluate every element in a product.
hforce :: xs :& h -> xs :& h
hforce p = hfoldrWithIndex (const seq) p p
{-# INLINE hforce #-}
