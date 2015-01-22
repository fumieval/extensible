{-# LANGUAGE PolyKinds #-}
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
  -- * Product
  (:*)(..)
  , (<:*)
  , (*++*)
  , hhead
  , htail
  , huncons
  , hmap
  , hzipWith
  , hzipWith3
  , hfoldMap
  , htraverse
  , htabulate
  , hlookup
  , sector
  , sectorAt
  , Generate(..)
  , Forall(..)) where

import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Unsafe.Coerce
import Data.Typeable
import Control.Applicative
import Data.Monoid

-- | The extensible product type
data (h :: k -> *) :* (s :: [k]) where
  Nil :: h :* '[]
  Tree :: !(h x)
    -> !(h :* Half xs)
    -> !(h :* Half (Tail xs))
    -> h :* (x ': xs)

deriving instance Typeable (:*)

instance Show (h :* '[]) where
  show Nil = "Nil"

instance (Show (h :* xs), Show (h x)) => Show (h :* (x ': xs)) where
  showsPrec d t = let (x, xs) = huncons t in showParen (d > 0) $
     showsPrec 0 x
    . showString " <:* "
    . showsPrec 0 xs

-- | /O(1)/ Extract the head element.
hhead :: h :* (x ': xs) -> h x
hhead (Tree a _ _) = a

-- | /O(n)/ Extract the tail of the product.
htail :: h :* (x ': xs) -> h :* xs
htail (Tree _ a@(Tree h _ _) b) = unsafeCoerce (Tree h) b (htail a)
htail _ = unsafeCoerce Nil

-- | Split a product to the head and the tail.
huncons :: forall h x xs. h :* (x ': xs) -> (h x, h :* xs)
huncons t@(Tree a _ _) = (a, htail t)

-- | /O(log n)/ Add an element to a product.
(<:*) :: forall h x xs. h x -> h :* xs -> h :* (x ': xs)
a <:* Tree b c d = Tree a (lemmaHalfTail (Proxy :: Proxy (Tail xs)) $! b <:* d) c
a <:* Nil = Tree a Nil Nil
infixr 0 <:*

-- | Transform every elements in a product, preserving the order.
hmap :: (forall x. g x -> h x) -> g :* xs -> h :* xs
hmap t (Tree h a b) = Tree (t h) (hmap t a) (hmap t b)
hmap _ Nil = Nil

-- | Serial combination of two products.
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
hlookup :: Position xs x -> h :* xs -> h x
hlookup = view . sectorAt
{-# INLINE hlookup #-}

-- | 'hmap' with its indices.
htabulate :: forall g h xs. (forall x. Position xs x -> g x -> h x) -> g :* xs -> h :* xs
htabulate f = go id where
  go :: (forall x. Position t x -> Position xs x) -> g :* t -> h :* t
  go k (Tree g a b) = Tree (f (k here) g) (go (k . navL) a) (go (k . navR) b)
  go _ Nil = Nil

-- | /O(log n)/ A lens for a specific element.
sector :: (Functor f, x ∈ xs) => (h x -> f (h x)) -> h :* xs -> f (h :* xs)
sector = sectorAt membership
{-# INLINE sector #-}

-- | /O(log n)/ A lens for a value in a known position.
sectorAt :: forall h x xs f. (Functor f) => Position xs x -> (h x -> f (h x)) -> h :* xs -> f (h :* xs)
sectorAt pos0 f = go pos0 where
  go :: forall t. Position t x -> h :* t -> f (h :* t)
  go pos (Tree h a b) = case navigate pos of
    Here -> fmap (\h' -> Tree h' a b) (f h)
    NavL p -> fmap (\a' -> Tree h a' b) $ go p a
    NavR p -> fmap (\b' -> Tree h a b') $ go p b
  go _ Nil = error "Impossible"
{-# INLINE sectorAt #-}

-- | Given a function that maps types to values, we can "collect" entities all you want.
class Generate (xs :: [k]) where
  generate :: (forall x. Position xs x -> h x) -> h :* xs

instance Generate '[] where
  generate _ = Nil
  {-# INLINE generate #-}

instance (Generate (Half xs), Generate (Half (Tail xs))) => Generate (x ': xs) where
  generate f = Tree (f here) (generate (f . navL)) (generate (f . navR))
  {-# INLINE generate #-}

-- | Guarantees the all elements satisfies the predicate.
class Forall c (xs :: [k]) where
  generateFor :: proxy c -> (forall x. c x => Position xs x -> h x) -> h :* xs

instance Forall c '[] where
  generateFor _ _ = Nil
  {-# INLINE generateFor #-}

instance (c x, Forall c (Half xs), Forall c (Half (Tail xs))) => Forall c (x ': xs) where
  generateFor proxy f = Tree (f here) (generateFor proxy (f . navL)) (generateFor proxy (f . navR))
  {-# INLINE generateFor #-}
