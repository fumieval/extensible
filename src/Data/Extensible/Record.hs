{-# LANGUAGE TemplateHaskell, PolyKinds, TypeFamilies, DataKinds, KindSignatures, FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances, Rank2Types #-}
module Data.Extensible.Record (FieldValue
  , Field(..)
  , Records
  , RecordLens
  , (@=)
  , mkField
  , (<:*)
  , (:*)(Nil)
  , module Data.Extensible.Inclusion
  , Labelable(..)
  , LabelPhantom
  ) where
import Data.Extensible.Product
import Data.Extensible.Internal
import Language.Haskell.TH
import GHC.TypeLits hiding (Nat)
import Data.Extensible.Inclusion
import Data.Proxy

-- | Associates
type family FieldValue (s :: Symbol) :: *

-- | The type of fields.
data Field (s :: Symbol) = Field { getField :: FieldValue s }

instance (KnownSymbol s, Show (FieldValue s)) => Show (Field s) where
  showsPrec d f@(Field a) = showParen (d >= 1) $ showString (symbolVal f)
    . showString " @= "
    . showsPrec 1 a

type RecordLens p f xs a = p a (f a) -> Records xs -> f (Records xs)

class Labelable s p where
  unlabel :: proxy s -> p a b -> a -> b

instance Labelable s (->) where
  unlabel _ = id
  {-# INLINE unlabel #-}

instance (s ~ t) => Labelable s (LabelPhantom t) where
  unlabel _ = error "Impossible"

data LabelPhantom s a b

-- | Annotate a value by the field name.
(@=) :: RecordLens (LabelPhantom s) Proxy '[s] (FieldValue s) -> FieldValue s -> Field s
(@=) _ = Field
{-# INLINE (@=) #-}
infix 1 @=

-- | The type of records which contain several
type Records = (:*) Field

-- | Generate a field.
-- @'mkField' "foo" Int@ defines:
-- @
-- type instance FieldValue "foo" = Int
-- foo :: (Functor f, "foo" ∈ s) => RecordLens f s "foo"
-- @
-- The yielding field is a <http://hackage.haskell.org/package/lens/docs/Control-Lens-Lens.html#t:Lens Lens>.
mkField :: String -> TypeQ -> DecsQ
mkField s t = do
  f <- newName "f"
  p <- newName "p"
  let st = litT (strTyLit s)
  let vt = conT ''FieldValue `appT` st
  let fcon = sigE (conE 'Field) $ arrowT `appT` vt `appT` (conT ''Field `appT` st)
  let lbl = conE 'Proxy `sigE` (conT ''Proxy `appT` st)
  let wf = varE '(.) `appE` (varE 'fmap `appE` fcon)
        `appE` (varE '(.) `appE` (varE 'unlabel `appE` lbl `appE` varE p) `appE` varE 'getField)
  let rn = mkName "s"
  sequence $ [tySynInstD ''FieldValue (tySynEqn [litT (strTyLit s)] t)
    , sigD (mkName s)
      $ forallT [PlainTV p, PlainTV f, PlainTV rn] (sequence [classP ''Functor [varT f]
        , classP ''Labelable [st, varT p]
        , classP ''Member [varT rn, st]])
      $ conT ''RecordLens `appT` varT p `appT` varT f `appT` varT rn `appT` t
    , funD (mkName s) [clause [varP p] (normalB $ varE 'sector `appE` wf) []]
    ]
