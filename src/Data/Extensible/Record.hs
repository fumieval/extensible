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
  , fieldOptic
  -- * Records and variants
  , RecordOf
  , Record
  , VariantOf
  , Variant
  -- * Internal
  , LabelPhantom
  , Labelling
  ) where
import Data.Extensible.Class
import Data.Extensible.Sum
import Data.Extensible.Product
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Language.Haskell.TH
import Data.Extensible.Inclusion
import Data.Extensible.Dictionary ()
import Control.Monad
import Data.Profunctor
import Data.Constraint
import Data.Extensible.Wrapper
import Data.Functor.Identity

-- | The type of records which contain several fields.
type Record = RecordOf Identity

-- | The dual of 'Record'
type Variant = VariantOf Identity

type RecordOf h = (:*) (Field h)

type VariantOf h = (:|) (Field h)

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

type FieldOptic k = forall f p q t xs (h :: kind -> *) (v :: kind) a. (Extensible f p q t
  , Associate k v xs
  , Labelling k p
  , Wrapper h v a)
  => p a (f a) -> q (t (Field h) xs) (f (t (Field h) xs))

-- | When you see this type as an argument, it expects a 'FieldLens'.
-- This type is used to resolve the name of the field internally.
type FieldName k = forall v. LabelPhantom k () (Proxy ())
  -> RecordOf Proxy '[k ':> v] -> Proxy (RecordOf Proxy '[k ':> v])

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
(@=) :: Wrapper h v a => FieldName k -> a -> Field h (k ':> v)
(@=) _ = Field . review _Wrapper
{-# INLINE (@=) #-}
infix 1 @=

-- | Lifted ('@=')
(<@=>) :: (Functor f, Wrapper h v a) => FieldName k -> f a -> Comp f (Field h) (k ':> v)
(<@=>) k = Comp . fmap (k @=)
{-# INLINE (<@=>) #-}
infix 1 <@=>

-- | Generate a field optic from the given name.
fieldOptic :: forall proxy k. proxy k -> FieldOptic k
fieldOptic k = pieceAssoc . withIso _Wrapper (\f g -> dimap (\(Field v) -> f v) (fmap (fieldNamed k . g)))
{-# INLINE fieldOptic #-}

fieldNamed :: proxy k -> h v -> Field h (k ':> v)
fieldNamed _ = Field

-- | Generate fields using 'fieldOptic'.
-- @'mkField' "foo bar"@ defines:
--
-- @
-- foo :: FieldOptic "foo"
-- bar :: FieldOptic "bar"
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
