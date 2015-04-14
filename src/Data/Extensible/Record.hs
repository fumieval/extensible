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
-- Flexible records and variants
-- Example: <https://github.com/fumieval/extensible/blob/master/examples/records.hs>
------------------------------------------------------------------------
module Data.Extensible.Record (
   module Data.Extensible.Class
  , module Data.Extensible.Inclusion
  , (@=)
  , (<@=>)
  , mkField
  , FieldOptic
  , FieldName
  -- * Records and variants
  , RecordOf
  , Record
  , emptyRecord
  , VariantOf
  , Variant
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
import Language.Haskell.TH
import Data.Extensible.Dictionary ()
import Control.Monad
import Data.Profunctor
import Data.Constraint
import Data.Extensible.Wrapper
import Data.Functor.Identity

-- | The type of records which contain several fields.
type RecordOf h = (:*) (Field h)

-- | The dual of 'RecordOf'
type VariantOf h = (:|) (Field h)

type Record = RecordOf Identity

type Variant = VariantOf Identity

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

type FieldOptic k = forall f p q t xs (h :: kind -> *) (v :: kind). (Extensible f p q t
  , Associate k v xs
  , Labelling k p
  , Wrapper h)
  => p (Repr h v) (f (Repr h v)) -> q (t (Field h) xs) (f (t (Field h) xs))

-- | The trivial inextensible data type
data Inextensible (h :: k -> *) (xs :: [k])

instance Functor f => Extensible f (->) (->) Inextensible where
  pieceAt _ _ _ = error "Impossible"

-- | When you see this type as an argument, it expects a 'FieldLens'.
-- This type is used to resolve the name of the field internally.
type FieldName k = forall v. LabelPhantom k () (Proxy ())
  -> Inextensible Proxy '[k ':> v] -> Proxy (Inextensible Proxy '[k ':> v])

type family Labelling s p :: Constraint where
  Labelling s (LabelPhantom t) = s ~ t
  Labelling s p = ()

-- | A ghostly type which spells the field name
data LabelPhantom s a b

instance Profunctor (LabelPhantom s) where
  dimap _ _ _ = error "Impossible"

instance Functor f => Extensible f (LabelPhantom s) q t where
  pieceAt _ _ = error "Impossible"

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

-- | Generate fields using 'itemAssoc'.
-- @'mkField' "foo bar"@ defines:
--
-- @
-- foo :: FieldOptic "foo"
-- foo = itemAssoc (Proxy :: Proxy "foo")
-- bar :: FieldOptic "bar"
-- bar = itemAssoc (Proxy :: Proxy "bar")
-- @
--
mkField :: String -> DecsQ
mkField str = fmap concat $ forM (words str) $ \s -> do
  let st = litT (strTyLit s)
  let lbl = conE 'Proxy `sigE` (conT ''Proxy `appT` st)
  sequence [sigD (mkName s) $ conT ''FieldOptic `appT` st
    , valD (varP (mkName s)) (normalB $ varE 'itemAssoc `appE` lbl) []
    , return $ PragmaD $ InlineP (mkName s) Inline FunLike AllPhases
    ]
