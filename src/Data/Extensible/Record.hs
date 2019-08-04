{-# LANGUAGE LambdaCase, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Record
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Bidirectional conversion from/to records
------------------------------------------------------------------------
module Data.Extensible.Record (IsRecord(..), toRecord, fromRecord, record) where

import Data.Extensible.Internal.Rig
import Data.Extensible.Product
import Data.Extensible.Field
import Data.Functor.Identity
import Data.Profunctor
import GHC.Generics
import GHC.TypeLits
import Type.Membership
import Type.Membership.HList

-- | The class of types that can be converted to/from a 'Record'.
class IsRecord a where
  type RecFields a :: [Assoc Symbol *]
  recordFromList :: HList (Field Identity) (RecFields a) -> a
  recordToList :: a -> HList (Field Identity) (RecFields a)

  type RecFields a = GRecFields (Rep a) '[]
  default recordFromList :: (Generic a, GIsRecord (Rep a) '[], GRecFields (Rep a) '[] ~ RecFields a) => HList (Field Identity) (RecFields a) -> a
  recordFromList xs = recordFromList' xs (\x (HNil :: HList (Field Identity) '[]) -> to x)

  default recordToList :: (Generic a, GIsRecord (Rep a) '[], GRecFields (Rep a) '[] ~ RecFields a) => a -> HList (Field Identity) (RecFields a)
  recordToList x = recordToList' (from x) HNil

instance IsRecord () where
  type RecFields () = '[]
  recordFromList _ = ()
  recordToList _ = HNil

-- | The class of types that can be converted to/from a 'Record'.
class GIsRecord f r where
  type GRecFields f (r :: [Assoc Symbol *]) :: [Assoc Symbol *]
  recordFromList' :: HList (Field Identity) (GRecFields f r) -> (f x -> HList (Field Identity) r -> cont) -> cont
  recordToList' :: f x -> HList (Field Identity) r -> HList (Field Identity) (GRecFields f r)

instance (GIsRecord f (GRecFields g r), GIsRecord g r) => GIsRecord (f :*: g) r where
  type GRecFields (f :*: g) r = GRecFields f (GRecFields g r)
  recordFromList' xs cont = recordFromList' xs $ \l ys -> recordFromList' ys $ \r zs -> cont (l :*: r) zs
  {-# INLINE recordFromList' #-}
  recordToList' (f :*: g) = recordToList' f . recordToList' g
  {-# INLINE recordToList' #-}

instance GIsRecord (S1 ('MetaSel ('Just name) su ss ds) (K1 i a)) r where
  type GRecFields (S1 ('MetaSel ('Just name) su ss ds) (K1 i a)) r = (name >: a) ': r
  recordFromList' (HCons (Field (Identity a)) xs) cont = cont (M1 (K1 a)) xs
  {-# INLINE recordFromList' #-}
  recordToList' (M1 (K1 a)) = HCons (Field (Identity a))
  {-# INLINE recordToList' #-}

instance GIsRecord f r => GIsRecord (C1 i f) r where
  type GRecFields (C1 i f) r = GRecFields f r
  recordFromList' xs cont = recordFromList' xs $ cont . M1
  {-# INLINE recordFromList' #-}
  recordToList' (M1 f) = recordToList' f
  {-# INLINE recordToList' #-}

instance GIsRecord f r => GIsRecord (D1 i f) r where
  type GRecFields (D1 i f) r = GRecFields f r
  recordFromList' xs cont = recordFromList' xs $ cont . M1
  {-# INLINE recordFromList' #-}
  recordToList' (M1 f) = recordToList' f
  {-# INLINE recordToList' #-}

-- | Convert a value into a 'Record'.
toRecord :: IsRecord a => a -> Record (RecFields a)
toRecord = fromHList . recordToList
{-# INLINE toRecord #-}

-- | Convert a 'Record' to a value.
fromRecord :: IsRecord a => Record (RecFields a) -> a
fromRecord = recordFromList . toHList
{-# INLINE fromRecord #-}

-- | @record :: IsRecord a => Iso' a (Record (RecFields a)) @
record :: (IsRecord a, Functor f, Profunctor p)
  => Optic' p f a (Record (RecFields a))
record = dimap toRecord (fmap fromRecord)
{-# INLINE record #-}
