{-# LANGUAGE CPP, TypeFamilies, MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Label
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Experimental API for OverloadedLabels. GHC 8.0+ only
-----------------------------------------------------------------------------
module Data.Extensible.Label where

import Data.Extensible.Class
import Data.Extensible.Field
import Data.Extensible.Product (hlookup)
import Data.Proxy
import GHC.OverloadedLabels
import GHC.Records
import Data.Extensible.Wrapper

instance k ~ l => IsLabel k (Proxy l) where
  fromLabel = Proxy

-- | Specialised version of 'itemAssoc'. Stands for "eXtensible LaBel"
xlb :: Proxy k -> FieldOptic k
xlb t = itemAssoc t

instance (Extensible f p e
  , Lookup xs k v
  , Labelling k p
  , Wrapper h
  , ExtensibleConstr e xs (Field h) (k ':> v)
  , rep ~ Repr h v
  , s ~ e xs (Field h)
  , s ~ t
  , rep ~ rep'
  )
  => IsLabel k (p rep (f rep') -> p s (f t)) where
  fromLabel = itemAssoc (Proxy :: Proxy k)

instance (Lookup xs k v, Wrapper h, Repr h v ~ a) => HasField k (RecordOf h xs) a where
  getField = unwrap . hlookup (association :: Membership xs (k >: v))
