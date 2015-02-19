{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Product
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
------------------------------------------------------------------------
module Data.Extensible.Product (
  -- * Basic operations
  (:*)(..)
  , (<:)
  , (<:*)
  , (*++*)
  , hhead
  , htail
  , huncons
  , hmap
  , hmapWithIndex
  , htrans
  , hzipWith
  , hzipWith3
  , hfoldMap
  , htraverse
  , hsequence
  , hcollect
  , hdistribute
  -- * Lookup
  , hlookup
  , hindex
  , sector
  , sectorAt
  -- * Generation
  , Generate(..)
  , htabulate
  , Forall(..)
  , htabulateFor) where

import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Unsafe.Coerce
import Control.Applicative
import Data.Monoid

-- | /O(1)/ Extract the head element.
hhead :: h :* (x ': xs) -> h x
hhead (Tree a _ _) = a
{-# INLINE hhead #-}

-- | /O(n)/ Extract the tail of the product.
-- FIXME: unsafeCoerce
htail :: h :* (x ': xs) -> h :* xs
htail (Tree _ a@(Tree h _ _) b) = unsafeCoerce (Tree h) b (htail a)
htail (Tree _ Nil _) = unsafeCoerce Nil

-- | Split a product to the head and the tail.
huncons :: forall h x xs. h :* (x ': xs) -> (h x, h :* xs)
huncons t@(Tree a _ _) = (a, htail t)
{-# INLINE huncons #-}

-- | An alias for ('<:').
(<:*) :: forall h x xs. h x -> h :* xs -> h :* (x ': xs)
a <:* Tree b c d = Tree a (lemmaHalfTail (Proxy :: Proxy (Tail xs)) $ b <: d) c
a <:* Nil = Tree a Nil Nil
infixr 0 <:*

-- | /O(log n)/ Add an element to a product.
(<:) :: h x -> h :* xs -> h :* (x ': xs)
(<:) = (<:*)
{-# INLINE (<:) #-}
infixr 0 <:

-- | Transform every elements in a product, preserving the order.
--
-- @
-- 'hmap' 'id' ≡ 'id'
-- 'hmap' (f . g) ≡ 'hmap' f . 'hmap' g
-- @
hmap :: (forall x. g x -> h x) -> g :* xs -> h :* xs
hmap t (Tree h a b) = Tree (t h) (hmap t a) (hmap t b)
hmap _ Nil = Nil

-- | Transform every elements in a product, preserving the order.
htrans :: (forall x. g x -> h (t x)) -> g :* xs -> h :* Map t xs
htrans t (Tree h a b) = unsafeCoerce (Tree (t h)) (htrans t a) (htrans t b)
htrans _ Nil = Nil

-- | Combine products.
(*++*) :: h :* xs -> h :* ys -> h :* (xs ++ ys)
(*++*) Nil ys = ys
(*++*) xs'@(Tree x _ _) ys = let xs = htail xs' in x <:* (xs *++* ys)
infixr 0 *++*

-- | 'zipWith' for heterogeneous product
hzipWith :: (forall x. f x -> g x -> h x) -> f :* xs -> g :* xs -> h :* xs
hzipWith t (Tree f a b) (Tree g c d) = Tree (t f g) (hzipWith t a c) (hzipWith t b d)
hzipWith _ Nil _ = Nil
hzipWith _ _ Nil = Nil

-- | 'zipWith3' for heterogeneous product
hzipWith3 :: (forall x. f x -> g x -> h x -> i x) -> f :* xs -> g :* xs -> h :* xs -> i :* xs
hzipWith3 t (Tree f a b) (Tree g c d) (Tree h e f') = Tree (t f g h) (hzipWith3 t a c e) (hzipWith3 t b d f')
hzipWith3 _ Nil _ _ = Nil
hzipWith3 _ _ Nil _ = Nil
hzipWith3 _ _ _ Nil = Nil

-- | Map elements to a monoid and combine the results.
--
-- @'hfoldMap' f . 'hmap' g ≡ 'hfoldMap' (f . g)@
hfoldMap :: Monoid a => (forall x. h x -> a) -> h :* xs -> a
hfoldMap f (Tree h a b) = f h <> hfoldMap f a <> hfoldMap f b
hfoldMap _ Nil = mempty

-- | Traverse all elements and combine the result sequentially.
-- @
-- htraverse (fmap f . g) ≡ fmap (hmap f) . htraverse g
-- htraverse pure ≡ pure
-- htraverse (Comp . fmap g . f) ≡ Comp . fmap (htraverse g) . htraverse f
-- @
htraverse :: Applicative f => (forall x. g x -> f (h x)) -> g :* xs -> f (h :* xs)
htraverse f (Tree h a b) = Tree <$> f h <*> htraverse f a <*> htraverse f b
htraverse _ Nil = pure Nil

-- | 'sequence' analog for extensible products
hsequence :: Applicative f => Comp f h :* xs -> f (h :* xs)
hsequence = htraverse getComp
{-# INLINE hsequence #-}

-- | The dual of 'htraverse'
hcollect :: (Functor f, Generate xs) => (a -> h :* xs) -> f a -> Comp f h :* xs
hcollect f m = htabulate $ \pos -> Comp $ fmap (hlookup pos . f) m
{-# INLINABLE hcollect #-}

-- | The dual of 'hsequence'
hdistribute :: (Functor f, Generate xs) => f (h :* xs) -> Comp f h :* xs
hdistribute = hcollect id
{-# INLINE hdistribute #-}

-- | /O(log n)/ Pick up an elemtnt.
hlookup :: Membership xs x -> h :* xs -> h x
hlookup = view . sectorAt
{-# INLINABLE hlookup #-}

-- | Flipped 'hlookup'
hindex :: h :* xs -> Membership xs x -> h x
hindex = flip hlookup
{-# INLINE hindex #-}

-- | 'hmap' with its indices.
hmapWithIndex :: forall g h xs. (forall x. Membership xs x -> g x -> h x) -> g :* xs -> h :* xs
hmapWithIndex f = go id where
  go :: (forall x. Membership t x -> Membership xs x) -> g :* t -> h :* t
  go k (Tree g a b) = Tree (f (k here) g) (go (k . navL) a) (go (k . navR) b)
  go _ Nil = Nil
{-# INLINE hmapWithIndex #-}

-- | /O(log n)/ A lens for a specific element.
sector :: forall h x xs. (x ∈ xs) => Lens' (h :* xs) (h x)
sector = lookupTree (Proxy :: Proxy (Head (Lookup x xs)))
{-# INLINE sector #-}

-- | /O(log n)/ A lens for a value in a known position.
sectorAt :: forall f h x xs. Functor f => Membership xs x -> (h x -> f (h x)) -> h :* xs -> f (h :* xs)
sectorAt pos f = flip go pos where
  go :: forall t. h :* t -> Membership t x -> f (h :* t)
  go (Tree h a b) = navigate
    (\Here -> fmap (\h' -> Tree h' a b) (f h))
    (fmap (\a' -> Tree h a' b) . go a)
    (fmap (\b' -> Tree h a b') . go b)
  go Nil = error "Impossible"
{-# INLINE sectorAt #-}

-- | Given a function that maps types to values, we can "collect" entities all you want.
class Generate (xs :: [k]) where
  -- | /O(n)/ htabulates a product with the given function.
  hgenerate :: Applicative f => (forall x. Membership xs x -> f (h x)) -> f (h :* xs)

instance Generate '[] where
  hgenerate _ = pure Nil
  {-# INLINE hgenerate #-}

instance (Generate (Half xs), Generate (Half (Tail xs))) => Generate (x ': xs) where
  hgenerate f = Tree <$> f here <*> hgenerate (f . navL) <*> hgenerate (f . navR)
  {-# INLINE hgenerate #-}

-- | Pure version of 'hgenerate'.
--
-- @
-- 'hmap' f ('htabulate' g) ≡ 'htabulate' (f . g)
-- 'htabulate' ('hindex' m) ≡ m
-- 'hindex' ('htabulate' k) ≡ k
-- @
htabulate :: Generate xs => (forall x. Membership xs x -> h x) -> h :* xs
htabulate f = getK0 (hgenerate (K0 . f))
{-# INLINE htabulate #-}

-- | Guarantees the all elements satisfies the predicate.
class Forall c (xs :: [k]) where
  -- | /O(n)/ Analogous to 'htabulate', but it also supplies a context @c x@ for every elements in @xs@.
  hgenerateFor :: Applicative f => proxy c -> (forall x. c x => Membership xs x -> f (h x)) -> f (h :* xs)

instance Forall c '[] where
  hgenerateFor _ _ = pure Nil
  {-# INLINE hgenerateFor #-}

instance (c x, Forall c (Half xs), Forall c (Half (Tail xs))) => Forall c (x ': xs) where
  hgenerateFor proxy f = Tree
    <$> f here
    <*> hgenerateFor proxy (f . navL)
    <*> hgenerateFor proxy (f . navR)
  {-# INLINE hgenerateFor #-}

-- | Pure version of 'hgenerateFor'.
htabulateFor :: Forall c xs => proxy c -> (forall x. c x => Membership xs x -> h x) -> h :* xs
htabulateFor p f = getK0 (hgenerateFor p (K0 . f))
{-# INLINE htabulateFor #-}
