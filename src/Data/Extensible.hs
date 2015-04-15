{-# LANGUAGE Safe #-}
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
-- This module just reexports everything.
-----------------------------------------------------------------------------
module Data.Extensible (
  module Data.Extensible.Class
  , module Data.Extensible.Dictionary
  , module Data.Extensible.Field
  , module Data.Extensible.Inclusion
  , module Data.Extensible.Match
  , module Data.Extensible.Nullable
  , module Data.Extensible.Plain
  , module Data.Extensible.Product
  , module Data.Extensible.Sum
  , module Data.Extensible.TH
  , module Data.Extensible.Union
  , module Data.Extensible.Wrapper
  ) where

import Data.Extensible.Class
import Data.Extensible.Dictionary
import Data.Extensible.Field
import Data.Extensible.Inclusion
import Data.Extensible.Match
import Data.Extensible.Nullable
import Data.Extensible.Plain
import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Extensible.TH
import Data.Extensible.Union
import Data.Extensible.Wrapper

