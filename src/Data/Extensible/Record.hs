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
   module Data.Extensible.Inclusion
  , Record
  , (<:)
  , (<:*)
  , (:*)(Nil)
  , (@=)
  , (<@=>)
  , mkField
  , Field(..)
  , getField
  , FieldLens
  , FieldName
  -- * Internal
  , Labelable(..)
  , LabelPhantom
  ) where
import Data.Extensible.Product
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Language.Haskell.TH
import GHC.TypeLits hiding (Nat)
import Data.Extensible.Inclusion
import Data.Extensible.Dictionary ()
import Control.Monad

-- | The type of fields.
data Field kv where
  Field :: v -> Field (k ':> v)

-- | Get a value of a field.
getField :: Field (k ':> v) -> v
getField (Field v) = v
{-# INLINE getField #-}

-- | The type of records which contain several fields.
type Record = (:*) Field

-- | Shows in @field \@= value@ style instead of the derived one.
instance (KnownSymbol k, Show v) => Show (Field (k ':> v)) where
  showsPrec d (Field a) = showParen (d >= 1) $ showString (symbolVal (Proxy :: Proxy k))
    . showString " @= "
    . showsPrec 1 a

-- | @FieldLens s@ is a type of lens that points a field named @s@.
--
-- @
-- 'FieldLens' "foo" = Associate "foo" a xs => Lens' ('Record' xs) a
-- @
--
type FieldLens k = forall f p xs v. (Functor f, Labelable k p, Associate k v xs)
  => p v (f v) -> Record xs -> f (Record xs)

-- | When you see this type as an argument, it expects a 'FieldLens'.
-- This type is used to resolve the name of the field internally.
type FieldName k = forall v. LabelPhantom k v (Proxy v)
  -> Record '[k ':> v] -> Proxy (Record '[k ':> v])

-- | A ghostly type which spells the field name
data LabelPhantom s a b

-- | An internal class to characterize 'FieldLens'
class Labelable s p where
  unlabel :: proxy s -> p a b -> a -> b

instance Labelable s (->) where
  unlabel _ = id
  {-# INLINE unlabel #-}

instance (s ~ t) => Labelable s (LabelPhantom t) where
  unlabel _ = error "Impossible"

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

type Assoc_ a b = a ':> b

-- | Generate field names.
-- @'mkField' "foo bar"@ defines:
--
-- @
-- foo :: FieldLens "foo"
-- foo :: FieldLens "bar"
-- @
--
-- The yielding fields are <http://hackage.haskell.org/package/lens/docs/Control-Lens-Lens.html#t:Lens Lens>es.
mkField :: String -> DecsQ
mkField str = fmap concat $ forM (words str) $ \s -> do
  f <- newName "f"
  let st = litT (strTyLit s)
  let vt = varT (mkName "v")
  let fcon = sigE (conE 'Field) $ forallT [PlainTV $ mkName "v"] (return [])
        $ arrowT `appT` vt `appT` (conT ''Field `appT` (conT ''Assoc_ `appT` st `appT` vt))
  let lbl = conE 'Proxy `sigE` (conT ''Proxy `appT` st)
  let wf = varE '(.) `appE` (varE 'fmap `appE` fcon)
        `appE` (varE '(.) `appE` (varE 'unlabel `appE` lbl `appE` varE f) `appE` varE 'getField)
  sequence [sigD (mkName s) $ conT ''FieldLens `appT` st
    , funD (mkName s) [clause [varP f] (normalB $ varE 'pieceAssoc `appE` wf) []]
    , return $ PragmaD $ InlineP (mkName s) Inline FunLike AllPhases
    ]
