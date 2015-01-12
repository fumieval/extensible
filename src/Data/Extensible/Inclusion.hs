{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Inclusion
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
------------------------------------------------------------------------
module Data.Extensible.Inclusion where

import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Extensible.Internal
import Data.Proxy

-- | Unicode alias for 'Include'
type xs ⊆ ys = Include ys xs

-- | @ys@ contains @xs@
type Include ys = Forall (Member ys)

-- | Reify the inclusion of type level sets.
inclusion :: forall xs ys. Include ys xs => Position ys :* xs
inclusion = generateFor (Proxy :: Proxy (Member ys)) (const membership)

-- | /O(m log n)/ Select some elements.
shrink :: (xs ⊆ ys) => h :* ys -> h :* xs
shrink h = hmap (\pos -> hlookup pos h) inclusion

-- | /O(log n)/ Embed to a larger union.
spread :: (xs ⊆ ys) => h :| xs -> h :| ys
spread (UnionAt pos h) = UnionAt (hlookup pos inclusion) h
