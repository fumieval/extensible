{-# LANGUAGE CPP, TypeFamilies, MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables #-}
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
import Data.Extensible.Wrapper
import Data.Extensible.Internal.Rig

instance k ~ l => IsLabel k (Proxy l) where
#if __GLASGOW_HASKELL__ >= 802
  fromLabel = Proxy
#else
  fromLabel _ = Proxy
#endif

-- | Specialised version of 'itemAssoc'.
訊 :: Proxy k -> FieldOptic k
訊 = itemAssoc

instance (Extensible f p t
  , Associate k v xs
  , Labelling k p
  , Wrapper h
  , rep ~ Repr h v)
  => IsLabel k (Optic' p f (t (Field h) xs) rep) where
#if __GLASGOW_HASKELL__ >= 802
  fromLabel = itemAssoc (Proxy :: Proxy k)
#else
  fromLabel _ = itemAssoc (Proxy :: Proxy k)
#endif
#endif
