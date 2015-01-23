{-# LANGUAGE TemplateHaskell, PolyKinds, TypeFamilies, FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances #-}
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
  , mkField
  , Field(..)
  , FieldValue
  , FieldLens
  , FieldName
  -- * Internal
  , Labelable(..)
  , LabelPhantom
  ) where
import Data.Extensible.Product
import Data.Extensible.Internal
import Language.Haskell.TH
import GHC.TypeLits hiding (Nat)
import Data.Extensible.Inclusion
import Data.Extensible.Dictionary ()

-- | Associates names with concrete types.
type family FieldValue (s :: Symbol) :: *

-- | The type of fields.
data Field (s :: Symbol) = Field { getField :: FieldValue s }

-- | The type of records which contain several fields.
type Record = (:*) Field

-- | Shows in @field \@= value@ style instead of the derived one.
instance (KnownSymbol s, Show (FieldValue s)) => Show (Field s) where
  showsPrec d f@(Field a) = showParen (d >= 1) $ showString (symbolVal f)
    . showString " @= "
    . showsPrec 1 a

-- | @FieldLens s@ is a type of lens that points a field named @s@.
--
-- @
-- 'FieldLens' s = (s ∈ xs) => Lens' ('Record' xs) ('FieldValue' s)
-- @
--
type FieldLens s = forall f p xs. (Functor f, Labelable s p, s ∈ xs)
  => p (FieldValue s) (f (FieldValue s)) -> Record xs -> f (Record xs)

-- | When you see this type as an argument, it expects a 'FieldLens'.
-- This type is used to resolve the name of the field internally.
type FieldName s = LabelPhantom s (FieldValue s) (Proxy (FieldValue s))
  -> Record '[s] -> Proxy (Record '[s])

-- | A ghostly type which reifies the field name
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
(@=) :: FieldName s -> FieldValue s -> Field s
(@=) _ = Field
{-# INLINE (@=) #-}
infix 1 @=

-- | Generate a field.
-- @'mkField' "foo" [t|Int|]@ defines:
--
-- @
-- type instance FieldValue "foo" = Int
--
-- foo :: FieldLens "foo"
-- @
--
-- The yielding field is a <http://hackage.haskell.org/package/lens/docs/Control-Lens-Lens.html#t:Lens Lens>.
mkField :: String -> TypeQ -> DecsQ
mkField s t = do
  f <- newName "f"
  let st = litT (strTyLit s)
  let vt = conT ''FieldValue `appT` st
  let fcon = sigE (conE 'Field) $ arrowT `appT` vt `appT` (conT ''Field `appT` st)
  let lbl = conE 'Proxy `sigE` (conT ''Proxy `appT` st)
  let wf = varE '(.) `appE` (varE 'fmap `appE` fcon)
        `appE` (varE '(.) `appE` (varE 'unlabel `appE` lbl `appE` varE f) `appE` varE 'getField)
  sequence [tySynInstD ''FieldValue (tySynEqn [litT (strTyLit s)] t)
    , sigD (mkName s)
      $ forallT [] (return [])
      $ conT ''FieldLens `appT` st
    , funD (mkName s) [clause [varP f] (normalB $ varE 'sector `appE` wf) []]
    , return $ PragmaD $ InlineP (mkName s) Inline FunLike AllPhases
    ]
