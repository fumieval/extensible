-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This package defines an extensible type-indexed product type and a union type.
-- Both are determined from the type-level list @[k]@
-- and a wrapper @k -> *@.
-- We can define ADTs not only for plain values, but also parameterized ones.
--
-- >>> let t = K0 (42 :: Int) <:* K0 "foo" <:* K0 (Just "bar") <:* Nil
-- >>> t
-- K0 42 <:* K0 "foo" <:* K0 (Just "bar") <:* Nil
-- >>> :t t
-- t :: K0 :* '[Int, [Char], Maybe [Char]]
-- >>> pluck t :: Int
-- 42
-----------------------------------------------------------------------------
module Data.Extensible (
  -- * Reexport
  module Data.Extensible.Inclusion
  , module Data.Extensible.Match
  , module Data.Extensible.Plain
  , module Data.Extensible.Product
  , module Data.Extensible.Sum
  -- * Lookup
  , Position
  , runPosition
  , (∈)()
  , Member(..)
  , ord
  ) where
import Data.Extensible.Internal
import Data.Extensible.Inclusion
import Data.Extensible.Match
import Data.Extensible.Plain
import Data.Extensible.Product
import Data.Extensible.Sum
-------------------------------------------------------------

