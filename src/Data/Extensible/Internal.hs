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
module Data.Extensible.Internal (Membership
  , getMemberId
  , runMembership
  , compareMembership
  , ord
  , NavHere(..)
  , navigate
  , here
  , navNext
  , navL
  , navR
  , (:*)(..)
  , Member(..)
  , remember
  , (∈)()
  , Nat(..)
  , ToInt(..)
  , Lookup
  , ListIndex
  , Assoc(..)
  , AssocKeys
  , Associate(..)
  , LookupTree(..)
  , Succ
  , MapSucc
  , Pred
  , Div2
  , Half
  , Head
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
newtype Membership (xs :: [k]) (x :: k) = Membership { getMemberId :: Word } deriving Typeable

newtype Remembrance xs x r = Remembrance (Member xs x => r)

-- | Remember that @Member xs x@ from 'Membership'.
remember :: forall xs x r. Membership xs x -> (Member xs x => r) -> r
remember pos r = unsafeCoerce (Remembrance r :: Remembrance xs x r) pos
{-# INLINE remember #-}

-- | Lookup types
type family ListIndex (n :: Nat) (xs :: [k]) :: k where
  ListIndex 'Zero (x ': xs) = x
  ListIndex ('SDNat n) (y ': xs) = ListIndex n (Half xs)
  ListIndex ('DNat n) xs = ListIndex n (Half xs)

type family Pred (n :: Nat) :: Nat where
  Pred ('SDNat 'Zero) = 'Zero
  Pred ('SDNat n) = 'DNat n
  Pred ('DNat n) = 'SDNat (Pred n)
  Pred 'Zero = 'Zero

type family Div2 (n :: Nat) :: Nat where
  Div2 ('SDNat n) = n
  Div2 ('DNat n) = n
  Div2 'Zero = 'Zero

-- | The kind of key-value pairs
data Assoc k v = k :> v

type family AssocKeys (xs :: [Assoc k v]) :: [k] where
  AssocKeys ((k ':> v) ': xs) = k ': AssocKeys xs
  AssocKeys '[] = '[]

-- | @'Associate' k v xs@ is essentially identical to @(k :> v) ∈ xs@
-- , but the type @v@ is inferred from @k@ and @xs@.
class Associate k v xs | k xs -> v where
  association :: Membership xs (k ':> v)

instance (Check k (Lookup k (AssocKeys xs)) ~ Expecting one, ToInt one, (k ':> v) ~ ListIndex one xs) => Associate k v xs where
  association = Membership (theInt (Proxy :: Proxy one))

-- | The type of extensible products.
data (h :: k -> *) :* (s :: [k]) where
  Nil :: h :* '[]
  Tree :: !(h x)
    -> h :* Half xs
    -> h :* Half (Tail xs)
    -> h :* (x ': xs)

deriving instance Typeable (:*)

class LookupTree (n :: Nat) (xs :: [k]) x | n xs -> x where
  lookupTree :: Functor f => proxy n
    -> (h x -> f (h x))
    -> h :* xs -> f (h :* xs)

instance LookupTree 'Zero (x ': xs) x where
  lookupTree _ f (Tree h a b) = fmap (\h' -> Tree h' a b) (f h)
  {-# INLINE lookupTree #-}

instance LookupTree n (Half xs) x => LookupTree ('SDNat n) (t ': xs) x where
  lookupTree _ f (Tree h a b) = fmap (\a' -> Tree h a' b) (lookupTree (Proxy :: Proxy n) f a)
  {-# INLINE lookupTree #-}

instance LookupTree (Pred n) (Half (Tail xs)) x => LookupTree ('DNat n) (t ': xs) x where
  lookupTree _ f (Tree h a b) = fmap (\b' -> Tree h a b')
    (lookupTree (Proxy :: Proxy (Div2 (Pred ('DNat n)))) (unsafeCoerce f) b)
  {-# INLINE lookupTree #-}

instance Show (Membership xs x) where
  show (Membership n) = "$(ord " ++ show n ++ ")"

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

class Member xs x where
  membership :: Membership xs x

-- | A type sugar to make type error more readable.
data Expecting a

-- | A type sugar to make type error more readable.
data Missing a

-- | A type sugar to make type error more readable.
data Ambiguous a

-- | Elaborate the result of 'Lookup'
type family Check x xs where
  Check x '[n] = Expecting n
  Check x '[] = Missing x
  Check x xs = Ambiguous x

instance (Check x (Lookup x xs) ~ Expecting one, ToInt one) => Member xs x where
  membership = Membership (theInt (Proxy :: Proxy one))
  {-# INLINE membership #-}

-- | Lookup types
type family Lookup (x :: k) (xs :: [k]) :: [Nat] where
  Lookup x (x ': xs) = 'Zero ': Lookup x xs
  Lookup x (y ': ys) = MapSucc (Lookup x ys)
  Lookup x '[] = '[]

-- | Interleaved list
type family Half (xs :: [k]) :: [k] where
  Half '[] = '[]
  Half (x ': y ': zs) = x ': Half zs
  Half (x ': '[]) = '[x]

-- | Type-level tail
type family Tail (xs :: [k]) :: [k] where
  Tail (x ': xs) = xs
  Tail '[] = '[]

-- | Type level binary number
data Nat = Zero | DNat Nat | SDNat Nat

-- | Converts type naturals into 'Word'.
class ToInt n where
  theInt :: proxy n -> Word

instance ToInt 'Zero where
  theInt _ = 0
  {-# INLINE theInt #-}

instance ToInt n => ToInt ('DNat n) where
  theInt _ = theInt (Proxy :: Proxy n) `unsafeShiftL` 1
  {-# INLINE theInt #-}

instance ToInt n => ToInt ('SDNat n) where
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

-- | GHC can't prove this
lemmaHalfTail :: proxy xs -> p (x ': Half (Tail xs)) -> p (Half (x ': xs))
lemmaHalfTail _ = unsafeCoerce
{-# INLINE lemmaHalfTail #-}

-- | GHC can't prove this
lemmaMerging :: p (Merge (Half xs) (Half (Tail xs))) -> p xs
lemmaMerging = unsafeCoerce

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
