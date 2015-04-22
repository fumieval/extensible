#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Record
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Flexible records and variants
-- Example: <https://github.com/fumieval/extensible/blob/master/examples/records.hs>
------------------------------------------------------------------------
module Data.Extensible.Field (
  Field(..)
  , (@=)
  , (<@=>)
  , FieldOptic
  , FieldName
  -- * Records and variants
  , RecordOf
  , Record
  , emptyRecord
  , VariantOf
  , Variant
  -- * Constraint
  , AssocKey
  , AssocValue
  , KeyValue
  -- * Internal
  , LabelPhantom
  , Labelling
  , Inextensible
  ) where
import Data.Extensible.Class
import Data.Extensible.Sum
import Data.Extensible.Product
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Profunctor.Unsafe
import Data.Constraint
import Data.Extensible.Wrapper
import Data.Functor.Identity
import GHC.TypeLits hiding (Nat)

type family AssocKey (kv :: Assoc k v) :: k where
  AssocKey (k ':> v) = k

type family AssocValue (kv :: Assoc k v) :: v where
  AssocValue (k ':> v) = v

class (pk (AssocKey kv), pv (AssocValue kv)) => KeyValue pk pv kv where

instance (pk k, pv v) => KeyValue pk pv (k ':> v)

-- | A @'Field' h (k ':> v)@ is @h v@, but is along with the index @k@.
--
-- @'Field' :: (v -> *) -> Assoc k v -> *@
--
newtype Field (h :: v -> *) (kv :: Assoc k v) = Field { getField :: h (AssocValue kv) }

instance Wrapper h => Wrapper (Field h) where
  type Repr (Field h) kv = Repr h (AssocValue kv)
  _Wrapper = dimap getField (fmap Field) . _Wrapper
  {-# INLINE _Wrapper #-}

-- | Shows in @field \@= value@ style instead of the derived one.
instance (KnownSymbol k, Wrapper h, Show (Repr h v)) => Show (Field h (k ':> v)) where
  showsPrec d (Field a) = showParen (d >= 1) $ showString (symbolVal (Proxy :: Proxy k))
    . showString " @= "
    . showsPrec 1 (view _Wrapper a)

-- | The type of records which contain several fields.
--
-- @RecordOf :: (v -> *) -> [Assoc k v] -> *@
--
type RecordOf h = (:*) (Field h)

-- | The dual of 'RecordOf'
--
-- @VariantOf :: (v -> *) -> [Assoc k v] -> *@
--
type VariantOf h = (:|) (Field h)

-- | Simple record
type Record = RecordOf Identity

-- | Simple variant
type Variant = VariantOf Identity

-- | An empty 'Record'.
emptyRecord :: Record '[]
emptyRecord = Nil
{-# INLINE emptyRecord #-}

-- | @FieldOptic s@ is a type of optics that points a field/constructor named @s@.
--
-- The yielding fields can be
-- <http://hackage.haskell.org/package/lens/docs/Control-Lens-Lens.html#t:Lens Lens>es
-- for 'Record's and
-- <http://hackage.haskell.org/package/lens/docs/Control-Lens-Lens.html#t:Prism Prism>s
-- for 'Variant's.
--
-- @
-- 'FieldOptic' "foo" = Associate "foo" a xs => Lens' ('Record' xs) a
-- 'FieldOptic' "foo" = Associate "foo" a xs => Prism' ('Variant' xs) a
-- @
--
-- 'FieldOptic's can be generated using 'mkField' defined in the "Data.Extensible.TH" module.
--
type FieldOptic k = forall f p t xs (h :: kind -> *) (v :: kind). (Extensible f p t
  , Associate k v xs
  , Labelling k p
  , Wrapper h)
  => Optic' p f (t (Field h) xs) (Repr h v)

-- | The trivial inextensible data type
data Inextensible (h :: k -> *) (xs :: [k])

instance (Functor f, Profunctor p) => Extensible f p Inextensible where
  pieceAt _ _ = error "Impossible"

-- | When you see this type as an argument, it expects a 'FieldLens'.
-- This type is used to resolve the name of the field internally.
type FieldName k = Optic' (LabelPhantom k) Proxy (Inextensible (Field Proxy) '[k ':> ()]) ()

type family Labelling s p :: Constraint where
  Labelling s (LabelPhantom t) = s ~ t
  Labelling s p = ()

-- | A ghostly type which spells the field name
data LabelPhantom s a b

instance Profunctor (LabelPhantom s) where
  dimap _ _ _ = error "Impossible"

-- | Annotate a value by the field name.
(@=) :: Wrapper h => FieldName k -> Repr h v -> Field h (k ':> v)
(@=) _ = Field #. review _Wrapper
{-# INLINE (@=) #-}
infix 1 @=

-- | Lifted ('@=')
(<@=>) :: (Functor f, Wrapper h) => FieldName k -> f (Repr h v) -> Comp f (Field h) (k ':> v)
(<@=>) k = Comp #. fmap (k @=)
{-# INLINE (<@=>) #-}
infix 1 <@=>