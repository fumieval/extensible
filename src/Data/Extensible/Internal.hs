{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns, StandaloneDeriving #-}
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
module Data.Extensible.Internal (
  -- * Membership
  Membership
  , getMemberId
  , mkMembership
  , reifyMembership
  , runMembership
  , compareMembership
  , impossibleMembership
  -- * Member class
  , Member(..)
  , remember
#if __GLASGOW_HASKELL__ >= 800
  , type (∈)
#else
  , (∈)()
#endif
  , FindType
  -- * Association
  , Assoc(..)
#if __GLASGOW_HASKELL__ >= 800
  , type (>:)
#else
  , (>:)()
#endif
  , Associate(..)
  , FindAssoc
  -- * Sugar
  , Elaborate
  , Elaborated(..)
  -- * Tree navigation
  , NavHere(..)
  , navigate
  , here
  , navNext
  , navL
  , navR
  -- * Miscellaneous
  , Nat(..)
  , KnownPosition(..)
  , Succ
  , Half
  , Head
  , Tail
  , Last
#if __GLASGOW_HASKELL__ >= 800
  , type (++)
#else
  , (++)()
#endif
  , Map
  , Merge
  , Concat
  , module Data.Type.Equality
  , module Data.Proxy
  ) where
import Data.Type.Equality
import Data.Proxy
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Word
#endif
import Control.Monad
import Unsafe.Coerce
import Data.Typeable
import Language.Haskell.TH hiding (Pred)
import Data.Bits

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
newtype Membership (xs :: [k]) (x :: k) = Membership { getMemberId :: Word } deriving Typeable

newtype Remembrance xs x r = Remembrance (Member xs x => r)

-- | Remember that @Member xs x@ from 'Membership'.
remember :: forall xs x r. Membership xs x -> (Member xs x => r) -> r
remember i r = unsafeCoerce (Remembrance r :: Remembrance xs x r) i
{-# INLINE remember #-}

class Member xs x where
  membership :: Membership xs x

instance (Elaborate x (FindType x xs) ~ 'Expecting pos, KnownPosition pos) => Member xs x where
  membership = Membership (theInt (Proxy :: Proxy pos))
  {-# INLINE membership #-}

reifyMembership :: Word -> (forall x. Membership xs x -> r) -> r
reifyMembership n k = k (Membership n)

-- | The kind of key-value pairs
data Assoc k v = k :> v
infix 0 :>

-- | A synonym for (':>')
type (>:) = (:>)

-- | @'Associate' k v xs@ is essentially identical to @(k :> v) ∈ xs@
-- , but the type @v@ is inferred from @k@ and @xs@.
class Associate k v xs | k xs -> v where
  association :: Membership xs (k ':> v)

instance (Elaborate k (FindAssoc k xs) ~ 'Expecting (n ':> v), KnownPosition n) => Associate k v xs where
  association = Membership (theInt (Proxy :: Proxy n))

data Elaborated k v = Expecting v | Missing k | Duplicate k

type family Elaborate (key :: k) (xs :: [v]) :: Elaborated k v where
  Elaborate k '[] = 'Missing k
  Elaborate k '[x] = 'Expecting x
  Elaborate k xs = 'Duplicate k

type family FindAssoc (key :: k) (xs :: [Assoc k v]) where
  FindAssoc k ((k ':> v) ': xs) = ('Zero ':> v) ': MapSuccKey (FindAssoc k xs)
  FindAssoc k ((k' ':> v) ': xs) = MapSuccKey (FindAssoc k xs)
  FindAssoc k '[] = '[]

type family MapSuccKey (xs :: [Assoc Nat v]) :: [Assoc Nat v] where
  MapSuccKey '[] = '[]
  MapSuccKey ((k ':> x) ': xs) = (Succ k ':> x) ': MapSuccKey xs

instance Show (Membership xs x) where
  show (Membership n) = "$(mkMembership " ++ show n ++ ")"

instance Eq (Membership xs x) where
  _ == _ = True

instance Ord (Membership xs x) where
  compare _ _ = EQ

-- | Embodies a type equivalence to ensure that the 'Membership' points the first element.
runMembership :: Membership (y ': xs) x -> (x :~: y -> r) -> (Membership xs x -> r) -> r
runMembership (Membership 0) l _ = l (unsafeCoerce Refl)
runMembership (Membership n) _ r = r (Membership (n - 1))
{-# INLINE runMembership #-}

-- | Compare two 'Membership's.
compareMembership :: Membership xs x -> Membership xs y -> Either Ordering (x :~: y)
compareMembership (Membership m) (Membership n) = case compare m n of
  EQ -> Right (unsafeCoerce Refl)
  x -> Left x
{-# INLINE compareMembership #-}

impossibleMembership :: Membership '[] x -> r
impossibleMembership _ = error "Impossible"

-- | PRIVILEGED: Navigate a tree.
navigate :: (NavHere xs x -> r)
  -> (Membership (Half (Tail xs)) x -> r)
  -> (Membership (Half (Tail (Tail xs))) x -> r)
  -> Membership xs x
  -> r
navigate h nl nr = \case
  Membership 0 -> h (unsafeCoerce Here)
  Membership n -> if n .&. 1 == 0
    then nr (Membership (unsafeShiftR (n - 1) 1))
    else nl (Membership (unsafeShiftR (n - 1) 1))
{-# INLINE navigate #-}

-- | Ensure that the first element of @xs@ is @x@
data NavHere xs x where
  Here :: NavHere (x ': xs) x

-- | The 'Membership' points the first element
here :: Membership (x ': xs) x
here = Membership 0
{-# INLINE here #-}

-- | The next membership
navNext :: Membership xs y -> Membership (x ': xs) y
navNext (Membership n) = Membership (n + 1)
{-# INLINE navNext #-}

-- | Describes the relation of 'Membership' within a tree
navL :: Membership (Half xs) y -> Membership (x ': xs) y
navL (Membership x) = Membership (x * 2 + 1)
{-# INLINE navL #-}

-- | Describes the relation of 'Membership' within a tree
navR :: Membership (Half (Tail xs)) y -> Membership (x ': xs) y
navR (Membership x) = Membership (x * 2 + 2)
{-# INLINE navR #-}

-- | Unicode flipped alias for 'Member'
type x ∈ xs = Member xs x

type family Head (xs :: [k]) :: k where
  Head (x ': xs) = x

-- | FindType types
type family FindType (x :: k) (xs :: [k]) :: [Nat] where
  FindType x (x ': xs) = 'Zero ': FindType x xs
  FindType x (y ': ys) = MapSucc (FindType x ys)
  FindType x '[] = '[]

-- | Interleaved list
type family Half (xs :: [k]) :: [k] where
  Half '[] = '[]
  Half (x ': y ': zs) = x ': Half zs
  Half (x ': '[]) = '[x]

-- | Type-level tail
type family Tail (xs :: [k]) :: [k] where
  Tail (x ': xs) = xs
  Tail '[] = '[]

type family Last (x :: [k]) :: k where
  Last '[x] = x
  Last (x ': xs) = Last xs

-- | Type level binary number
data Nat = Zero | DNat Nat | SDNat Nat

-- | Converts type naturals into 'Word'.
class KnownPosition n where
  theInt :: proxy n -> Word

instance KnownPosition 'Zero where
  theInt _ = 0
  {-# INLINE theInt #-}

instance KnownPosition n => KnownPosition ('DNat n) where
  theInt _ = theInt (Proxy :: Proxy n) `unsafeShiftL` 1
  {-# INLINE theInt #-}

instance KnownPosition n => KnownPosition ('SDNat n) where
  theInt _ = (theInt (Proxy :: Proxy n) `unsafeShiftL` 1) + 1
  {-# INLINE theInt #-}

-- | The successor of the number
type family Succ (x :: Nat) :: Nat where
  Succ 'Zero = 'SDNat 'Zero
  Succ ('DNat n) = 'SDNat n
  Succ ('SDNat n) = 'DNat (Succ n)

-- | Ideally, it will be 'Map Succ'
type family MapSucc (xs :: [Nat]) :: [Nat] where
  MapSucc '[] = '[]
  MapSucc (x ': xs) = Succ x ': MapSucc xs

-- | Type level map
type family Map (f :: k -> k) (xs :: [k]) :: [k] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

-- | Type level ++
type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': xs ++ ys

infixr 5 ++

-- | Type level concat
type family Concat (xs :: [[k]]) :: [k] where
  Concat '[] = '[]
  Concat (x ': xs) = x ++ Concat xs

-- | Type level merging
type family Merge (xs :: [k]) (ys :: [k]) :: [k] where
  Merge (x ': xs) (y ': ys) = x ': y ': Merge xs ys
  Merge xs '[] = xs
  Merge '[] ys = ys
