{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.HList
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Heterogeneous list
------------------------------------------------------------------------
module Data.Extensible.HList where

import Data.Extensible.Internal
import Unsafe.Coerce

data HList (h :: k -> *) (xs :: [k]) where
  HNil :: HList h '[]
  HCons :: h x -> HList h xs -> HList h (x ': xs)

infixr 5 `HCons`

htraverse :: Applicative f => (forall x. g x -> f (h x)) -> HList g xs -> f (HList h xs)
htraverse _ HNil = pure HNil
htraverse f (HCons h xs) = HCons <$> f h <*> htraverse f xs

htraverseWithIndex :: forall f g h xs. Applicative f
    => (forall x. Membership xs x -> g x -> f (h x)) -> HList g xs -> f (HList h xs)
htraverseWithIndex f = go 0 where
  go :: Int -> HList g t -> f (HList h t)
  go !k (HCons x xs) = HCons <$> f (unsafeCoerce k) x <*> go (k + 1) xs
  go _ HNil = pure HNil
{-# INLINE htraverseWithIndex #-}

hfoldrWithIndex :: forall h r xs. (forall x. Membership xs x -> h x -> r -> r) -> r -> HList h xs -> r
hfoldrWithIndex f r = go 0 where
  go :: Int -> HList h t -> r
  go !k (HCons x xs) = f (unsafeCoerce k) x $ go (k + 1) xs
  go _ HNil = r
{-# INLINE hfoldrWithIndex #-}

hlength :: HList h xs -> Int
hlength = go 0 where
  go :: Int -> HList h xs -> Int
  go n HNil = n
  go n (HCons _ xs) = let !n' = n + 1 in go n' xs
{-# INLINE hlength #-}
