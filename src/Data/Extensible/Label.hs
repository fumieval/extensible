{-# LANGUAGE CPP, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Label
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Experimental API for OverloadedLabels. GHC 8.0+ only
-----------------------------------------------------------------------------
module Data.Extensible.Label where

#if __GLASGOW_HASKELL__ >= 800

import Data.Extensible.Class
import Data.Extensible.Field
import Data.Proxy
import GHC.OverloadedLabels

instance k ~ l => IsLabel k (Proxy l) where
  fromLabel _ = Proxy

-- | Specialised version of 'itemAssoc'.
訊 :: Proxy k -> FieldOptic k
訊 = itemAssoc

#endif
