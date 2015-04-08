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
  , _K0
  , AllOf
  , OneOf
  , (<%)
  , pluck
  , bury
  , (<%|)
  , accessing
  , decFields
  , decFieldsDeriving
  ) where
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Extensible.Class
import Data.Extensible.Product
import Data.Extensible.Sum
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
pluck = views piece getK0
{-# INLINE pluck #-}

-- | Embed a plain value.
bury :: (x ∈ xs) => x -> OneOf xs
bury = embed . K0
{-# INLINE bury #-}

-- | Naive pattern matching for a plain value.
(<%|) :: (x -> r) -> (OneOf xs -> r) -> OneOf (x ': xs) -> r
(<%|) = unsafeCoerce (<:|)
infixr 1 <%|

-- | An accessor for newtype constructors.
accessing :: (Coercible b a, b ∈ xs) => (a -> b) -> Lens' (AllOf xs) a
accessing c f = piece (_K0 (fmap c . f . coerce))
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
#if MIN_VERSION_template_haskell(2,10,0)
          $ forallT (PlainTV xs : tvs) (sequence [conT ''Member `appT` varT xs `appT` conT name_])
#else
          $ forallT (PlainTV xs : tvs) (sequence [classP ''Member [varT xs, conT name_]])
#endif
          $ conT ''Lens' `appT` (conT ''AllOf `appT` varT xs) `appT` return ty
        , valD (varP name) (normalB $ varE 'accessing `appE` conE nc) []
        , return $ PragmaD $ InlineP name Inline FunLike AllPhases
        ]
    mkBody (TySynD name_ tvs ty) = mkBody (NewtypeD [] name_ tvs (NormalC (mkName (nameBase name_)) [(NotStrict, ty)]) [])
    mkBody _ = fail "Unsupported declaration: genField handles newtype declarations or type synonyms"
