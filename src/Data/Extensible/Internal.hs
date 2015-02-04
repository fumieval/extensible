{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Inclusion
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A bunch of combinators that contains magic
------------------------------------------------------------------------
module Data.Extensible.Internal (Membership
  , runMembership
  , compareMembership
  , ord
  , NavHere(..)
  , navigate
  , here
  , navNext
  , navL
  , navR
  , Member(..)
  , (∈)()
  , Nat(..)
  , ToInt(..)
  , Lookup
  , Succ
  , MapSucc
  , Half
  , Tail
  , lemmaHalfTail
  , lemmaMerging
  , (++)()
  , Map
  , Merge
  , Concat
  , Check
  , Expecting
  , Missing
  , Ambiguous
  , module Data.Type.Equality
  , module Data.Proxy
  ) where
import Data.Type.Equality
import Data.Proxy
import Control.Applicative
import Control.Monad
import Unsafe.Coerce
import Data.Typeable
import Language.Haskell.TH
import Control.DeepSeq
import Data.Word
import Data.Bits

-- | Generates a 'Membership' that corresponds to the given ordinal (0-origin).
ord :: Int -> Q Exp
ord n = do
  let names = map mkName $ take (n + 1) $ concatMap (flip replicateM ['a'..'z']) [1..]
  let rest = mkName "any"
  let cons x xs = PromotedConsT `AppT` x `AppT` xs
  let t = foldr cons (VarT rest) (map VarT names)
  sigE (conE 'Membership `appE` litE (IntegerL $ toInteger n))
    $ forallT (PlainTV rest : map PlainTV names) (pure [])
    $ conT ''Membership `appT` pure t `appT` varT (names !! n)

-- | The position of @x@ in the type level set @xs@.
newtype Membership (xs :: [k]) (x :: k) = Membership Word8 deriving Typeable

instance NFData (Membership xs x) where
  rnf (Membership a) = rnf a

instance Show (Membership xs x) where
  show (Membership n) = "$(ord " ++ show n ++ ")"

instance Eq (Membership xs x) where
  _ == _ = True

instance Ord (Membership xs x) where
  compare _ _ = EQ

-- | Embodies a type equivalence to ensure that the 'Membership' points the first element.
runMembership :: Membership (y ': xs) x -> Either (x :~: y) (Membership xs x)
runMembership (Membership 0) = Left (unsafeCoerce Refl)
runMembership (Membership n) = Right (Membership (n - 1))
{-# INLINE runMembership #-}

-- | PRIVILEGED: Compare two 'Membership's.
compareMembership :: Membership xs x -> Membership xs y -> Either Ordering (x :~: y)
compareMembership (Membership m) (Membership n) = case compare m n of
  EQ -> Right (unsafeCoerce Refl)
  x -> Left x
{-# INLINE compareMembership #-}

-- | PRIVILEGED: Navigate a tree.
navigate :: (NavHere xs x -> r)
  -> (Membership (Half (Tail xs)) x -> r)
  -> (Membership (Half (Tail (Tail xs))) x -> r)
  -> Membership xs x
  -> r
navigate h nl nr = \case
  Membership 0 -> h (unsafeCoerce Here)
  Membership n -> let !x = n - 1 in if testBit x 0
    then nr (Membership (shiftR x 1))
    else nl (Membership (shiftR x 1))
{-# INLINE navigate #-}

-- | Ensure that the first element of @xs@ is @x@
data NavHere xs x where
  Here :: NavHere (x ': xs) x

here :: Membership (x ': xs) x
here = Membership 0
{-# INLINE here #-}

navNext :: Membership xs y -> Membership (x ': xs) y
navNext (Membership n) = Membership (n + 1)
{-# INLINE navNext #-}

navL :: Membership (Half xs) y -> Membership (x ': xs) y
navL (Membership x) = Membership (x * 2 + 1)
{-# INLINE navL #-}

navR :: Membership (Half (Tail xs)) y -> Membership (x ': xs) y
navR (Membership x) = Membership (x * 2 + 2)
{-# INLINE navR #-}

-- | Unicode flipped alias for 'Member'
type x ∈ xs = Member xs x

-- | @Member x xs@ or @x ∈ xs@ indicates that @x@ is an element of @xs@.
class Member (xs :: [k]) (x :: k) where
  membership :: Membership xs x

-- | A type sugar to make type error more readable.
data Expecting a

-- | A type sugar to make type error more readable.
data Missing a

-- | A type sugar to make type error more readable.
data Ambiguous a

type family Check x xs where
  Check x '[n] = Expecting n
  Check x '[] = Missing x
  Check x xs = Ambiguous x

instance (Check x (Lookup x xs) ~ Expecting one, ToInt one) => Member xs x where
  membership = Membership (theInt (Proxy :: Proxy one))
  {-# INLINE membership #-}

type family Half (xs :: [k]) :: [k] where
  Half '[] = '[]
  Half (x ': y ': zs) = x ': Half zs
  Half (x ': '[]) = '[x]

type family Tail (xs :: [k]) :: [k] where
  Tail (x ': xs) = xs
  Tail '[] = '[]

data Nat = Zero | DNat Nat | SDNat Nat

class ToInt n where
  theInt :: proxy n -> Word8

instance ToInt Zero where
  theInt _ = 0
  {-# INLINE theInt #-}

instance ToInt n => ToInt (DNat n) where
  theInt _ = theInt (Proxy :: Proxy n) `shiftL` 1
  {-# INLINE theInt #-}

instance ToInt n => ToInt (SDNat n) where
  theInt _ = (theInt (Proxy :: Proxy n) `shiftL` 1) + 1
  {-# INLINE theInt #-}

type family Lookup (x :: k) (xs :: [k]) :: [Nat] where
  Lookup x (x ': xs) = Zero ': Lookup x xs
  Lookup x (y ': ys) = MapSucc (Lookup x ys)
  Lookup x '[] = '[]

type family Succ (x :: Nat) :: Nat where
  Succ Zero = SDNat Zero
  Succ (DNat n) = SDNat n
  Succ (SDNat n) = DNat (Succ n)

type family MapSucc (xs :: [Nat]) :: [Nat] where
  MapSucc '[] = '[]
  MapSucc (x ': xs) = Succ x ': MapSucc xs

-- | GHC can't prove this
lemmaHalfTail :: proxy xs -> p (x ': Half (Tail xs)) -> p (Half (x ': xs))
lemmaHalfTail _ = unsafeCoerce
{-# INLINE lemmaHalfTail #-}

-- | GHC can't prove this
lemmaMerging :: p (Merge (Half xs) (Half (Tail xs))) -> p xs
lemmaMerging = unsafeCoerce

type family Map (f :: k -> k) (xs :: [k]) :: [k] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': xs ++ ys

infixr 5 ++

type family Concat (xs :: [[k]]) :: [k] where
  Concat '[] = '[]
  Concat (x ': xs) = x ++ Concat xs

type family Merge (xs :: [k]) (ys :: [k]) :: [k] where
  Merge (x ': xs) (y ': ys) = x ': y ': Merge xs ys
  Merge xs '[] = xs
  Merge '[] ys = ys
