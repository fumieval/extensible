{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
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
  , htraverseWithIndex
  , hsequence
  , hcollect
  , hdistribute
  -- * Lookup
  , hlookup
  , hindex
  -- * Generation
  , Generate(..)
  , htabulate
  , Forall(..)
  , htabulateFor) where

import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Unsafe.Coerce
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Data.Monoid
import Data.Typeable (Typeable)
import Data.Extensible.Class
import Data.Functor.Identity
import Data.Extensible.Wrapper
import Data.Profunctor.Unsafe
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Control.Comonad

-- | The type of extensible products.
--
-- @(:*) :: (k -> *) -> [k] -> *@
--
data (h :: k -> *) :* (s :: [k]) where
  Nil :: h :* '[]
  Tree :: !(h x)
    -> h :* Half xs
    -> h :* Half (Tail xs)
    -> h :* (x ': xs)

deriving instance Typeable (:*)

-- | /O(1)/ Extract the head element.
hhead :: h :* (x ': xs) -> h x
hhead (Tree a _ _) = a
{-# INLINE hhead #-}

-- | /O(log n)/ Extract the tail of the product.
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
hcollect f m = htabulate $ \i -> Comp $ fmap (hlookup i . f) m
{-# INLINABLE hcollect #-}

-- | The dual of 'hsequence'
hdistribute :: (Functor f, Generate xs) => f (h :* xs) -> Comp f h :* xs
hdistribute = hcollect id
{-# INLINE hdistribute #-}

-- | /O(log n)/ Pick up an elemtnt.
hlookup :: Membership xs x -> h :* xs -> h x
hlookup = view . pieceAt
{-# INLINABLE hlookup #-}

-- | Flipped 'hlookup'
hindex :: h :* xs -> Membership xs x -> h x
hindex = flip hlookup
{-# INLINE hindex #-}

-- | 'hmap' with 'Membership's.
hmapWithIndex :: forall g h xs. (forall x. Membership xs x -> g x -> h x) -> g :* xs -> h :* xs
hmapWithIndex f = go id where
  go :: (forall x. Membership t x -> Membership xs x) -> g :* t -> h :* t
  go k (Tree g a b) = Tree (f (k here) g) (go (k . navL) a) (go (k . navR) b)
  go _ Nil = Nil
{-# INLINE hmapWithIndex #-}

-- | 'htraverse' with 'Membership's.
htraverseWithIndex :: forall f g h xs. Applicative f
  => (forall x. Membership xs x -> g x -> f (h x)) -> g :* xs -> f (h :* xs)
htraverseWithIndex f = go id where
  go :: (forall x. Membership t x -> Membership xs x) -> g :* t -> f (h :* t)
  go k (Tree g a b) = Tree <$> f (k here) g <*> go (k . navL) a <*> go (k . navR) b
  go _ Nil = pure Nil
{-# INLINE htraverseWithIndex #-}

instance (Corepresentable p, Comonad (Corep p), Functor f) => Extensible f p (:*) where
  -- | /O(log n)/ A lens for a value in a known position.
  pieceAt = pieceAt_
  {-# INLINE pieceAt #-}

pieceAt_ :: forall (xs :: [k]) (x :: k) (h :: k -> *) (f :: * -> *) (p :: * -> * -> *).
  (Functor f, Corepresentable p, Comonad (Corep p))
  => Membership xs x -> p (h x) (f (h x)) -> p (h :* xs) (f (h :* xs))
pieceAt_ i f = cotabulate $ flip go i where
  go :: forall t. Corep p (h :* t) -> Membership t x -> f (h :* t)
  go w@(extract -> Tree h a b) = navigate
    (\Here -> fmap (\h' -> Tree h' a b) (cosieve f (h <$ w)))
    (fmap (\a' -> Tree h a' b) . go (a <$ w))
    (fmap (\b' -> Tree h a b') . go (b <$ w))
  go _ = error "Impossible"
{-# INLINE pieceAt_ #-}

-- | Given a function that maps types to values, we can "collect" entities all you want.
class Generate (xs :: [k]) where
  -- | /O(n)/ Generate a product with the given function.
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
htabulate f = runIdentity (hgenerate (Identity #. f))
{-# INLINE htabulate #-}

-- | Guarantees the all elements satisfies the predicate.
class Forall c (xs :: [k]) where
  -- | /O(n)/ Analogous to 'hgenerate', but it also supplies a context @c x@ for every elements in @xs@.
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
htabulateFor p f = runIdentity (hgenerateFor p (Identity #. f))
{-# INLINE htabulateFor #-}

-- | GHC can't prove this
lemmaHalfTail :: proxy xs -> h :* (x ': Half (Tail xs)) -> h :* (Half (x ': xs))
lemmaHalfTail _ = unsafeCoerce
{-# INLINE lemmaHalfTail #-}
