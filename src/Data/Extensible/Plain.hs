{-# LANGUAGE LambdaCase, TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Plain
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
------------------------------------------------------------------------
module Data.Extensible.Plain (
  K0(..)
  , AllOf
  , OneOf
  , (<%)
  , pluck
  , bury
  , (<%|)
  , record
  , recordAt
  , (<?%)
  , K1(..)
  , (<?!)
  , accessing
  , decFields
  , decFieldsDeriving
  )where
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Typeable
import Unsafe.Coerce
import Language.Haskell.TH hiding (Match(..))
import Data.Char
import Data.Coerce

-- | Alias for plain products
type AllOf xs = K0 :* xs

-- | Alias for plain sums
type OneOf xs = K0 :| xs

-- | /O(log n)/ Add a plain value to a product.
(<%) :: x -> AllOf xs -> AllOf (x ': xs)
(<%) = unsafeCoerce (<:*)
{-# INLINE (<%) #-}
infixr 5 <%

-- | Extract a plain value.
pluck :: (x ∈ xs) => AllOf xs -> x
pluck = views sector getK0
{-# INLINE pluck #-}

-- | Embed a plain value.
bury :: (x ∈ xs) => x -> OneOf xs
bury = embed . K0
{-# INLINE bury #-}

-- | Naive pattern matching for a plain value.
(<%|) :: (x -> r) -> (OneOf xs -> r) -> OneOf (x ': xs) -> r
(<%|) = unsafeCoerce (<:|)
infixr 1 <%|

-- | /O(log n)/ A lens for a plain value in a product.
record :: (x ∈ xs, Functor f) => (x -> f x) -> (AllOf xs -> f (AllOf xs))
record f = sector $ unsafeCoerce f `asTypeOf` (fmap K0 . f . getK0)
{-# INLINE record #-}

-- | /O(log n)/ A lens for a plain value in a product.
recordAt :: (Functor f) => Membership xs x -> (x -> f x) -> (AllOf xs -> f (AllOf xs))
recordAt pos f = sectorAt pos $ unsafeCoerce f `asTypeOf` (fmap K0 . f . getK0)
{-# INLINE recordAt #-}

-- | Prepend a clause for a plain value.
(<?%) :: (x -> a) -> Match K0 a :* xs -> Match K0 a :* (x ': xs)
(<?%) = unsafeCoerce (<:*)
{-# INLINE (<?%) #-}
infixr 1 <?%

-- | Wrap a type that has a kind @* -> *@.
newtype K1 a f = K1 { getK1 :: f a } deriving (Eq, Ord, Read, Typeable)

-- | Prepend a clause for a parameterized value.
(<?!) :: (f x -> a) -> Match (K1 x) a :* xs -> Match (K1 x) a :* (f ': fs)
(<?!) = unsafeCoerce (<:*)
infixr 1 <?!

-- | An accessor for newtype constructors.
accessing :: (Coercible b a, b ∈ xs) => (a -> b) -> Lens' (AllOf xs) a
accessing c f = record (fmap c . f . coerce)
{-# INLINE accessing #-}

-- | Generate newtype wrappers and lenses from type synonyms.
--
-- @
-- decFields [d|type Foo = Int|]
-- @
--
-- Generates:
--
-- @
-- newtype Foo = Foo Int
-- foo :: (Foo ∈ xs) => Lens' (AllOf xs) Int
-- foo = accessing Foo
-- @
--
decFields :: DecsQ -> DecsQ
decFields = decFieldsDeriving []

-- | 'decFields' with additional deriving clauses
decFieldsDeriving :: [Name] -> DecsQ -> DecsQ
decFieldsDeriving drv' ds = ds >>= fmap concat . mapM mkBody
  where
    mkBody (NewtypeD cx name_ tvs (NormalC nc [(st, ty)]) drv) = do
      let name = let (x:xs) = nameBase name_ in mkName (toLower x : xs)
      xs <- newName "xs"
      sequence [return $ NewtypeD cx name_ tvs (NormalC nc [(st, ty)]) (drv' ++ drv)
        ,sigD name
          $ forallT (PlainTV xs : tvs) (sequence [classP ''Member [varT xs, conT name_]])
          $ conT ''Lens' `appT` (conT ''AllOf `appT` varT xs) `appT` return ty
        , valD (varP name) (normalB $ varE 'accessing `appE` conE nc) []
        , return $ PragmaD $ InlineP name Inline FunLike AllPhases
        ]
    mkBody (TySynD name_ tvs ty) = mkBody (NewtypeD [] name_ tvs (NormalC (mkName (nameBase name_)) [(NotStrict, ty)]) [])
    mkBody _ = fail "Unsupported declaration: genField handles newtype declarations or type synonyms"
