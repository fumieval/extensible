{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeFamilies, FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances #-}
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
-- Flexible records with well-typed fields.
-- Example: <https://github.com/fumieval/extensible/blob/master/examples/records.hs>
------------------------------------------------------------------------
module Data.Extensible.Record (
   module Data.Extensible.Class
  , module Data.Extensible.Inclusion
  , (@=)
  , (<@=>)
  , mkField
  , Field(..)
  , getField
  , FieldOptic
  , FieldName
  , fieldOptic
  -- * Records and variants
  , Record
  , (<:)
  , (:*)(Nil)
  , Variant
  -- * Internal
  , LabelPhantom
  ) where
import Data.Extensible.Class
import Data.Extensible.Sum
import Data.Extensible.Product
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Language.Haskell.TH
import GHC.TypeLits hiding (Nat)
import Data.Extensible.Inclusion
import Data.Extensible.Dictionary ()
import Control.Monad
import Data.Tagged
import Data.Profunctor
import Data.Constraint

-- | The type of fields.
data Field kv where
  Field :: v -> Field (k ':> v)

-- | Get a value of a field.
getField :: Field (k ':> v) -> v
getField (Field v) = v
{-# INLINE getField #-}

-- | The type of records which contain several fields.
type Record = (:*) Field

-- | The dual of 'Record'
type Variant = (:|) Field

-- | Shows in @field \@= value@ style instead of the derived one.
instance (KnownSymbol k, Show v) => Show (Field (k ':> v)) where
  showsPrec d (Field a) = showParen (d >= 1) $ showString (symbolVal (Proxy :: Proxy k))
    . showString " @= "
    . showsPrec 1 a

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
type FieldOptic k = forall f p q t xs v. (Functor f
  , Profunctor p
  , Extensible f p q t
  , Associate k v xs
  , Labelling k p)
  => p v (f v) -> q (t Field xs) (f (t Field xs))

-- | When you see this type as an argument, it expects a 'FieldLens'.
-- This type is used to resolve the name of the field internally.
type FieldName k = forall v. LabelPhantom k v (Proxy v)
  -> Record '[k ':> v] -> Proxy (Record '[k ':> v])

type family Labelling s p :: Constraint where
  Labelling s (LabelPhantom t) = s ~ t
  Labelling s p = ()

-- | A ghostly type which spells the field name
data LabelPhantom s a b

instance Profunctor (LabelPhantom s) where
  dimap _ _ _ = error "Impossible"

instance Extensible f (LabelPhantom s) q t where
  pieceAt _ _ = error "Impossible"

-- | Annotate a value by the field name.
(@=) :: FieldName k -> v -> Field (k ':> v)
(@=) _ = Field
{-# INLINE (@=) #-}
infix 1 @=

-- | Lifted ('@=')
(<@=>) :: Functor f => FieldName k -> f v -> Comp f Field (k ':> v)
(<@=>) _ = comp Field
{-# INLINE (<@=>) #-}
infix 1 <@=>

fieldOptic :: forall proxy k. proxy k -> FieldOptic k
fieldOptic _ = pieceAssoc . dimap getField (fmap (Field :: v -> Field (k ':> v)))
{-# INLINE fieldOptic #-}

-- | Generate fields using 'fieldOptic'.
-- @'mkField' "foo bar"@ defines:
--
-- @
-- foo :: FieldOptic "foo"
-- foo :: FieldOptic "bar"
-- @
--
mkField :: String -> DecsQ
mkField str = fmap concat $ forM (words str) $ \s -> do
  let st = litT (strTyLit s)
  let lbl = conE 'Proxy `sigE` (conT ''Proxy `appT` st)
  sequence [sigD (mkName s) $ conT ''FieldOptic `appT` st
    , valD (varP (mkName s)) (normalB $ varE 'fieldOptic `appE` lbl) []
    , return $ PragmaD $ InlineP (mkName s) Inline FunLike AllPhases
    ]
