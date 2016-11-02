{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Product.Ordered
-- Copyright   :  (c) Hidenori Azuma, 2016
--                    (Adapted from '../Product.hs' (c) Fumiaki Kinoshita, 2015)
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Several functions of h :* xs defined at Data.Extensible.Product module
-- does not consider the type ordering of parameter xs
-- when they construct Monoid or Applicative composition.
--
-- This module provides another version of them that respects the order
-- even for Monoid or Applicative effects.
--
-- But it takes some run time cost. So it's your choice which one you use.
--
-- To use ordered version, you need to import this module explicitly.
--
-- @
-- import Data.Extensible (htraverse)
-- import qualified Data.Extensible.Product.Ordered as Xo
--
-- ...
--
-- ... = htraverse f record -- doesn't consider the order of effects
--
-- ... = Xo.htraverse f record -- does consider the order of effects
-- @
--
-----------------------------------------------------------------------------

module Data.Extensible.Product.Ordered (
  -- * Ordered variations of basic operations
  hfoldMap
  , hfoldMapWithIndex
  , htraverse
  , htraverseWithIndex
  , hsequence
  , hgenerate
  , hgenerateFor) where

import Data.Extensible.Internal
-- import Data.Extensible.Internal.Rig
import Data.Extensible.Product ((:*)(..))
import qualified Data.Extensible.Product as Xu
import Unsafe.Coerce
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Data.Typeable (Typeable)
import Data.Extensible.Wrapper
import Data.Profunctor.Unsafe

--
-- List structure product (only for internal use)
--
data ListProd (h :: k -> *) (s :: [k]) where
  ListNil :: ListProd h '[]
  ListCons :: !(h x) -> ListProd h xs -> ListProd h (x ': xs)

deriving instance Typeable ListProd

fromListProd :: ListProd h k -> h :* k
fromListProd ListNil = Nil
fromListProd (ListCons x xs) = let (a, b) = interleave xs in Tree x (fromListProd a) (fromListProd b)
  where
    interleave :: ListProd h xs -> (ListProd h (Half xs), ListProd h (Half (Tail xs)))
    interleave ListNil = (ListNil, ListNil)
    interleave (ListCons a b0) = case b0 of
      ListCons b rest ->
        let (as, bs) = interleave rest in (ListCons a as, unsafeCoerce $ ListCons b bs)
      ListNil -> (ListCons a ListNil, ListNil)
{-# INLINE[1] fromListProd #-}

toListProd :: h :* k -> ListProd h k
toListProd Nil = ListNil
toListProd (Tree h a0 b0) = ListCons h (unInterleave (toListProd a0) (toListProd b0))
  where
    unInterleave ::
      forall h xs ys. Tail xs ~ ys =>
      ListProd h (Half xs) -> ListProd h (Half ys) -> ListProd h xs
    unInterleave (ListCons a as) bs =
      unsafeCoerce $ ListCons a (unInterleave bs (unsafeCoerce as) :: ListProd h ys)
    unInterleave ListNil _ = unsafeCoerce ListNil
{-# INLINE [1] toListProd #-}

traverseListProd :: Applicative f => (forall x. g x -> f (h x)) -> ListProd g xs -> f (ListProd h xs)
traverseListProd _ ListNil = pure ListNil
traverseListProd f (ListCons a as) = ListCons <$> f a <*> traverseListProd f as

traverseWithIndexListProd :: forall f g h xs. Applicative f
  => (forall x. Membership xs x -> g x -> f (h x)) -> ListProd g xs -> f (ListProd h xs)
traverseWithIndexListProd f = go id
  where
    go :: (forall x. Membership t x -> Membership xs x) -> ListProd g t -> f (ListProd h t)
    go k (ListCons a as) = ListCons <$> f (k here) a <*> go (k . navNext) as
    go _ ListNil = pure ListNil
{-# INLINE traverseWithIndexListProd #-}

foldMapListProd :: Monoid a => (forall x. g x -> a) -> ListProd g xs -> a
foldMapListProd _ ListNil = mempty
foldMapListProd f (ListCons a as) = f a `mappend` foldMapListProd f as

foldMapWithIndexListProd :: forall g a xs. Monoid a
  => (forall x. Membership xs x -> g x -> a) -> ListProd g xs -> a
foldMapWithIndexListProd f = go id
  where
    go :: (forall x. Membership t x -> Membership xs x) -> ListProd g t -> a
    go k (ListCons a as) = f (k here) a `mappend` go (k . navNext) as
    go _ ListNil = mempty
{-# INLINE foldMapWithIndexListProd #-}

mapListProd :: (forall x. g x -> h x) -> ListProd g xs -> ListProd h xs
mapListProd _ ListNil = ListNil
mapListProd f (ListCons a as) = f a `ListCons` mapListProd f as

mapWithIndexListProd :: forall g h xs. (forall x. Membership xs x -> g x -> h x)
  -> ListProd g xs -> ListProd h xs
mapWithIndexListProd f = go id
  where
    go :: (forall x. Membership t x -> Membership xs x) -> ListProd g t -> ListProd h t
    go k (ListCons a as) = f (k here) a `ListCons` go (k . navNext) as
    go _ ListNil = ListNil

{-# RULES
"fromListProd . toListProd" forall l. fromListProd (toListProd l) = l
"toListProd . fromListProd" forall l. toListProd (fromListProd l) = l
"toListProd . hmap . fromListProd"
    forall (f :: forall x. g x -> h x) l. toListProd (Xu.hmap f (fromListProd l)) = mapListProd f l
"toListProd . hmapWithIndex . fromListProd"
    forall (f :: forall x. Membership xs x -> g x -> h x) l.
        toListProd (Xu.hmapWithIndex f (fromListProd l)) = mapWithIndexListProd f l
 #-}


--
-- Reimplementation of Data.Extensible.Product
--

-- | Map elements to a monoid and combine the results.
--
-- @'hfoldMap' f . 'hmap' g ≡ 'hfoldMap' (f . g)@
hfoldMap :: Monoid a => (forall x. h x -> a) -> h :* xs -> a
hfoldMap f = foldMapListProd f . toListProd
{-# INLINE hfoldMap #-}


-- | 'hfoldMap' with the membership of elements.
hfoldMapWithIndex :: forall g a xs. Monoid a
  => (forall x. Membership xs x -> g x -> a) -> g :* xs -> a
hfoldMapWithIndex f = foldMapWithIndexListProd f . toListProd


-- | Traverse all elements and combine the result sequentially.
-- @
-- htraverse (fmap f . g) ≡ fmap (hmap f) . htraverse g
-- htraverse pure ≡ pure
-- htraverse (Comp . fmap g . f) ≡ Comp . fmap (htraverse g) . htraverse f
-- @
htraverse :: Applicative f => (forall x. g x -> f (h x)) -> g :* xs -> f (h :* xs)
htraverse f = (fromListProd <$>) . traverseListProd f . toListProd
{-# INLINE htraverse #-}


-- | 'htraverse' with 'Membership's.
htraverseWithIndex :: forall f g h xs. Applicative f
  => (forall x. Membership xs x -> g x -> f (h x)) -> g :* xs -> f (h :* xs)
htraverseWithIndex f = (fromListProd <$>) . traverseWithIndexListProd f . toListProd
{-# INLINE htraverseWithIndex #-}


-- | 'sequence' analog for extensible products
hsequence :: Applicative f => Comp f h :* xs -> f (h :* xs)
hsequence = htraverse getComp
{-# INLINE hsequence #-}


-- | /O(n)/ Generate a product with the given function.
hgenerate :: (Xu.Generate xs, Applicative f) =>
             (forall x. Membership xs x -> f (h x)) -> f (h :* xs)
hgenerate f = hsequence $ Xu.htabulate (Comp #. f)
{-# INLINE hgenerate #-}


-- | /O(n)/ Analogous to 'hgenerate', but it also supplies a context @c x@ for every elements in @xs@.
hgenerateFor :: (Xu.Forall c xs, Applicative f) => proxy c ->
                (forall x. c x => Membership xs x -> f (h x)) -> f (h :* xs)
hgenerateFor proxy f = hsequence $ Xu.htabulateFor proxy (Comp #. f)
{-# INLINE hgenerateFor #-}

