{-# LANGUAGE PolyKinds, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Data.Extensible.Internal.HList
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
--
------------------------------------------------------------------------
module Data.Extensible.Internal.HList where

import Data.Extensible.Internal

data HList (h :: k -> *) (s :: [k]) where
  HNil :: HList h '[]
  HCons :: h x -> HList h xs -> HList h (x ': xs)

infixr 5 `HCons`

merge :: HList h xs -> HList h ys -> HList h (Merge xs ys)
merge (HCons x xs) (HCons y ys) = x `HCons` y `HCons` merge xs ys
merge HNil ys = ys
merge xs HNil = xs

split :: forall h xs. HList h xs -> (HList h (Half xs), HList h (Half (Tail xs)))
split (HCons x (HCons y ys)) = let (a, b) = split ys
  in (HCons x a, lemmaHalfTail (Proxy :: Proxy (Tail (Tail xs))) $ HCons y b)
split (HCons x HNil) = (HCons x HNil, HNil)
split HNil = (HNil, HNil)
