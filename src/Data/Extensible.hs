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
-- Extensible ADTs provided by this module are determined from a type-level list @[k]@
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
  , module Data.Extensible.League
  , module Data.Extensible.Match
  , module Data.Extensible.Plain
  , module Data.Extensible.Record
  , module Data.Extensible.Product
  , module Data.Extensible.Sum
  , module Data.Extensible.Union
  ) where
import Data.Extensible.Inclusion
import Data.Extensible.Match
import Data.Extensible.Plain
import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Extensible.League
import Data.Extensible.Union
import Data.Extensible.Record
import Data.Extensible.Dictionary ()

-------------------------------------------------------------

