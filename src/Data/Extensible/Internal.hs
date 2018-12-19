{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Inclusion
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- A bunch of combinators that contains magic
------------------------------------------------------------------------
module Data.Extensible.Internal (
  -- * Membership
  Membership
  , getMemberId
  , mkMembership
  , reifyMembership
  , leadership
  , compareMembership
  , impossibleMembership
  , here
  , navNext
  -- * Member class
  , Member(..)
  , remember
  , type (∈)
  , FindType
  -- * Association
  , Assoc(..)
  , type (>:)
  , Associate(..)
  , FindAssoc
  -- * Sugar
  , Elaborate
  , Elaborated(..)
  -- * Miscellaneous
  , Head
  , Last
  , module Data.Type.Equality
  , module Data.Proxy
  ) where
import Control.DeepSeq (NFData)
import Data.Type.Equality
import Data.Proxy
import Control.Monad
import Unsafe.Coerce
import Data.Hashable
import Data.Text.Prettyprint.Doc
import Data.Typeable
import Language.Haskell.TH hiding (Pred)
import Language.Haskell.TH.Lift
import Data.Semigroup (Semigroup(..))
import GHC.TypeLits

-- | Generates a 'Membership' that corresponds to the given ordinal (0-origin).
mkMembership :: Int -> Q Exp
mkMembership n = do
  let names = map mkName $ take (n + 1) $ concatMap (flip replicateM ['a'..'z']) [1..]
  let rest = mkName "any"
  let cons x xs = PromotedConsT `AppT` x `AppT` xs
  let t = foldr cons (VarT rest) (map VarT names)
  sigE (conE 'Membership `appE` litE (IntegerL $ toInteger n))
    $ forallT (PlainTV rest : map PlainTV names) (pure [])
    $ conT ''Membership `appT` pure t `appT` varT (names !! n)

-- | The position of @x@ in the type level set @xs@.
newtype Membership (xs :: [k]) (x :: k) = Membership
  { getMemberId :: Int -- ^ get the position as an 'Int'.
  } deriving (Typeable, NFData)

instance Lift (Membership xs x) where
  lift (Membership i) = mkMembership i

newtype Remembrance xs x r = Remembrance (Member xs x => r)

-- | Remember that @Member xs x@ from 'Membership'.
remember :: forall xs x r. Membership xs x -> (Member xs x => r) -> r
remember i r = unsafeCoerce (Remembrance r :: Remembrance xs x r) i
{-# INLINE remember #-}

-- | @x@ is a member of @xs@
class Member xs x where
  membership :: Membership xs x

instance (Elaborate x (FindType x xs) ~ 'Expecting pos, KnownNat pos) => Member xs x where
  membership = Membership (fromInteger $ natVal (Proxy :: Proxy pos))
  {-# INLINE membership #-}

instance Hashable (Membership xs x) where
  hashWithSalt s = hashWithSalt s . getMemberId

-- | Make up a 'Membership' from an integer.
reifyMembership :: Int -> (forall x. Membership xs x -> r) -> r
reifyMembership n k = k (Membership n)

-- | The kind of key-value pairs
data Assoc k v = k :> v
infix 0 :>

-- | A synonym for (':>')
type (>:) = '(:>)

-- | @'Associate' k v xs@ is essentially identical to @(k :> v) ∈ xs@
-- , but the type @v@ is inferred from @k@ and @xs@.
class Associate k v xs | k xs -> v where
  association :: Membership xs (k ':> v)

instance (Elaborate k (FindAssoc 0 k xs) ~ 'Expecting (n ':> v), KnownNat n) => Associate k v xs where
  association = Membership (fromInteger $ natVal (Proxy :: Proxy n))

-- | A readable type search result
data Elaborated k v = Expecting v | Missing k | Duplicate k

-- | Make the result more readable
type family Elaborate (key :: k) (xs :: [v]) :: Elaborated k v where
  Elaborate k '[] = 'Missing k
  Elaborate k '[x] = 'Expecting x
  Elaborate k xs = 'Duplicate k

-- | Find a type associated to the specified key.
type family FindAssoc (n :: Nat) (key :: k) (xs :: [Assoc k v]) where
  FindAssoc n k ((k ':> v) ': xs) = (n ':> v) ': FindAssoc (1 + n) k xs
  FindAssoc n k ((k' ':> v) ': xs) = FindAssoc (1 + n) k xs
  FindAssoc n k '[] = '[]

instance Show (Membership xs x) where
  show (Membership n) = "$(mkMembership " ++ show n ++ ")"

instance Pretty (Membership xs x) where
  pretty (Membership n) = "$(mkMembership " <> pretty n <> ")"

instance Eq (Membership xs x) where
  _ == _ = True

instance Ord (Membership xs x) where
  compare _ _ = EQ

instance Semigroup (Membership xs x) where
  x <> _ = x

-- | Embodies a type equivalence to ensure that the 'Membership' points the first element.
leadership :: Membership (y ': xs) x -> (x :~: y -> r) -> (Membership xs x -> r) -> r
leadership (Membership 0) l _ = l (unsafeCoerce Refl)
leadership (Membership n) _ r = r (Membership (n - 1))
{-# INLINE leadership #-}

-- | Compare two 'Membership's.
compareMembership :: Membership xs x -> Membership xs y -> Either Ordering (x :~: y)
compareMembership (Membership m) (Membership n) = case compare m n of
  EQ -> Right (unsafeCoerce Refl)
  x -> Left x
{-# INLINE compareMembership #-}

-- | There is no 'Membership' of an empty list.
impossibleMembership :: Membership '[] x -> r
impossibleMembership _ = error "Impossible"

-- | The 'Membership' points the first element
here :: Membership (x ': xs) x
here = Membership 0
{-# INLINE here #-}

-- | The next membership
navNext :: Membership xs y -> Membership (x ': xs) y
navNext (Membership n) = Membership (n + 1)
{-# INLINE navNext #-}

-- | Unicode flipped alias for 'Member'
type x ∈ xs = Member xs x

-- | First element
type family Head (xs :: [k]) :: k where
  Head (x ': xs) = x

-- | FindType types
type family FindType (x :: k) (xs :: [k]) :: [Nat] where
  FindType x (x ': xs) = 0 ': FindType x xs
  FindType x (y ': ys) = MapSucc (FindType x ys)
  FindType x '[] = '[]

-- | Last element
type family Last (x :: [k]) :: k where
  Last '[x] = x
  Last (x ': xs) = Last xs

-- | Ideally, it will be 'Map Succ'
type family MapSucc (xs :: [Nat]) :: [Nat] where
  MapSucc '[] = '[]
  MapSucc (x ': xs) = (1 + x) ': MapSucc xs
