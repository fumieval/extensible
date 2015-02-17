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
  , htrans
  , hzipWith
  , hzipWith3
  , hfoldMap
  , htraverse
  , htabulate
  -- * Lookup
  , hlookup
  , sector
  , sectorAt
  -- * Generation
  , Generate(..)
  , generate
  , Forall(..)
  , generateFor
  -- * HList
  , fromHList
  , toHList) where

import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Extensible.Internal.HList
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

-- | Combine all elements.
hfoldMap :: Monoid a => (forall x. h x -> a) -> h :* xs -> a
hfoldMap f (Tree h a b) = f h <> hfoldMap f a <> hfoldMap f b
hfoldMap _ Nil = mempty

-- | Traverse all elements.
htraverse :: Applicative f => (forall x. h x -> f (h x)) -> h :* xs -> f (h :* xs)
htraverse f (Tree h a b) = Tree <$> f h <*> htraverse f a <*> htraverse f b
htraverse _ Nil = pure Nil

-- | /O(log n)/ Pick up an elemtnt.
hlookup :: Membership xs x -> h :* xs -> h x
hlookup = view . sectorAt
{-# INLINE hlookup #-}

-- | 'hmap' with its indices.
htabulate :: forall g h xs. (forall x. Membership xs x -> g x -> h x) -> g :* xs -> h :* xs
htabulate f = go id where
  go :: (forall x. Membership t x -> Membership xs x) -> g :* t -> h :* t
  go k (Tree g a b) = Tree (f (k here) g) (go (k . navL) a) (go (k . navR) b)
  go _ Nil = Nil
{-# INLINE htabulate #-}

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
  -- | /O(n)/ generates a product with the given function.
  generateA :: Applicative f => (forall x. Membership xs x -> f (h x)) -> f (h :* xs)

instance Generate '[] where
  generateA _ = pure Nil
  {-# INLINE generateA #-}

instance (Generate (Half xs), Generate (Half (Tail xs))) => Generate (x ': xs) where
  generateA f = Tree <$> f here <*> generateA (f . navL) <*> generateA (f . navR)
  {-# INLINE generateA #-}

-- | Pure version of 'generateA'.
generate :: Generate xs => (forall x. Membership xs x -> h x) -> h :* xs
generate f = getK0 (generateA (K0 . f))

-- | Guarantees the all elements satisfies the predicate.
class Forall c (xs :: [k]) where
  -- | /O(n)/ Analogous to 'generate', but it also supplies a context @c x@ for every elements in @xs@.
  generateForA :: Applicative f => proxy c -> (forall x. c x => Membership xs x -> f (h x)) -> f (h :* xs)

instance Forall c '[] where
  generateForA _ _ = pure Nil
  {-# INLINE generateForA #-}

instance (c x, Forall c (Half xs), Forall c (Half (Tail xs))) => Forall c (x ': xs) where
  generateForA proxy f = Tree
    <$> f here
    <*> generateForA proxy (f . navL)
    <*> generateForA proxy (f . navR)
  {-# INLINE generateForA #-}

-- | Pure version of 'generateForA'.
generateFor :: Forall c xs => proxy c -> (forall x. c x => Membership xs x -> h x) -> h :* xs
generateFor p f = getK0 (generateForA p (K0 . f))

-- | Turn a product into 'HList'.
toHList :: h :* xs -> HList h xs
toHList (Tree h a b) = HCons h $ lemmaMerging $ toHList a `merge` toHList b
toHList Nil = HNil

-- | Build a product from 'HList'.
fromHList :: HList h xs -> h :* xs
fromHList (HCons x xs) = let (a, b) = split xs in Tree x (fromHList a) (fromHList b)
fromHList HNil = Nil
