{-# LANGUAGE DataKinds, TypeOperators, PolyKinds, KindSignatures, GADTs, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Extensible (
  -- * Lookup
  Position
  , (∈)(..)
  -- * Product
  , (:*)(..)
  , (<:*)
  , unconsP
  , outP
  , record
  , recordAt
  -- * Sum
  , (:|)(..)
  , (<:|)
  , exhaust
  , inS
  -- * Utilities
  , K0(..)
  , platter
  , (<%)
  , K1(..)
  , liftU
  , Union(..)
  , Match(..)
  , match
  ) where
import Unsafe.Coerce
import Data.Bits
import Data.Typeable
import Control.Applicative

data (h :: k -> *) :* (s :: [k]) where
  Nil :: h :* '[]
  Tree :: h x
    -> h :* Half xs
    -> h :* Half (Tail xs)
    -> h :* (x ': xs)

instance Show (h :* '[]) where
  show Nil = "Nil"

instance (Show (h :* xs), Show (h x)) => Show (h :* (x ': xs)) where
  showsPrec d (unconsP -> (x, xs)) = showParen (d > 10) $
     showsPrec 6 x
    . showString " <:* "
    . showsPrec 6 xs

unconsP :: h :* (x ': xs) -> (h x, h :* xs)
unconsP (Tree a Nil _) = (a, unsafeCoerce Nil)
unconsP (Tree a bd c) = (a, let (b, d) = unconsP (unsafeCoerce bd) in unsafeCoerce $ Tree b (unsafeCoerce c) d)

(<:*) :: h x -> h :* xs -> h :* (x ': xs)
a <:* Nil = Tree a Nil Nil
a <:* Tree b c d = Tree a (unsafeCoerce (<:*) b d) c --  (Half (x1 : xs1) ~ (x1 : Half (Tail xs1)))
infixr 5 <:*

outP :: forall h x xs. (x ∈ xs) => h :* xs -> h x
outP = getConst . recordAt (position :: Position x xs) Const

record :: forall h x xs f. (Functor f, x ∈ xs) => (h x -> f (h x)) -> h :* xs -> f (h :* xs)
record = recordAt (position :: Position x xs)

view :: ((a -> Const a a) -> (s -> Const a s)) -> s -> a
view l = getConst . l Const

recordAt :: forall h x xs f. (Functor f) => Position x xs -> (h x -> f (h x)) -> h :* xs -> f (h :* xs)
recordAt (Position x) f = go x where
  go 0 (Tree h a b) = fmap (\h' -> Tree h' a b) $ unsafeCoerce f $ h
  go n (Tree h a b) = case divMod (n - 1) 2 of
    (m, 0) -> fmap (\a' -> Tree h a' b) $ unsafeCoerce go m a
    (m, _) -> fmap (\b' -> Tree h a b') $ unsafeCoerce go m b
  go _ Nil = error "Impossible"

inS :: (x ∈ xs) => h x -> h :| xs
inS = UnionAt position

(<:|) :: (h x -> r) -> (h :| xs -> r) -> h :| (x ': xs) -> r
(<:|) r _ (UnionAt (Position 0) h) = r (unsafeCoerce h)
(<:|) _ c (UnionAt (Position n) h) = c $ unsafeCoerce $ UnionAt (Position (n - 1)) h

exhaust :: h :| '[] -> r
exhaust _ = error "Impossible"

data (h :: k -> *) :| (s :: [k]) where
  UnionAt :: Position x xs -> h x -> h :| xs

instance Show (h :| '[]) where
  show _ = undefined

instance (Show (h x), Show (h :| xs)) => Show (h :| (x ': xs)) where
  showsPrec d = (\h -> showParen (d > 10) $ showString "inS " . showsPrec d h)
    <:| showsPrec d

newtype K0 a = K0 { getK0 :: a } deriving (Eq, Ord, Read, Typeable)

instance Show a => Show (K0 a) where
  showsPrec d (K0 a) = showParen (d > 10) $ showString "K0 " . showsPrec 11 a

platter :: (x ∈ xs, Functor f) => (x -> f x) -> (K0 :* xs -> f (K0 :* xs))
platter f = record (fmap K0 . f . getK0)

newtype K1 a f = K1 { getK1 :: f a } deriving (Eq, Ord, Read, Typeable)

instance Show (f a) => Show (K1 a f) where
  showsPrec d (K1 a) = showParen (d > 10) $ showString "K1 " . showsPrec 11 a

newtype Match h a x = Match { runMatch :: h x -> a }

match :: Match h a :* xs -> h :| xs -> a
match p (UnionAt pos h) = runMatch (view (recordAt pos) p) h

(<%) :: x -> K0 :* xs -> K0 :* (x ': xs)
(<%) = unsafeCoerce (<:*)
infixr 5 <%

newtype Union fs a = Union { getUnion :: K1 a :| fs }

liftU :: (f ∈ fs) => f a -> Union fs a
liftU = Union . inS . K1
{-# INLINE liftU #-}

deriving instance Show (K1 a :| fs) => Show (Union fs a)

---------------------------------------------------------------------

newtype Position x xs = Position Int deriving Show

class (x :: k) ∈ (xs :: [k]) where
  position :: Position x xs

instance Record (Lookup x xs) => x ∈ xs where
  position = Position $ theInt (Proxy :: Proxy (Lookup x xs))
  {-# INLINE position #-}

type family Half (xs :: [k]) :: [k] where
  Half '[] = '[]
  Half (x ': y ': zs) = x ': zs
  Half (x ': '[]) = x ': '[]

type family Tail (xs :: [k]) :: [k] where
  Tail (x ': xs) = xs
  Tail '[] = '[]

data Nat = Zero | DNat Nat | SDNat Nat | NotFound

class Record n where
  theInt :: Proxy n -> Int

instance Record Zero where
  theInt _ = 0

instance Record n => Record (DNat n) where
  theInt _ = theInt (Proxy :: Proxy n) `shiftL` 1

instance Record n => Record (SDNat n) where
  theInt _ = (theInt (Proxy :: Proxy n) `shiftL` 1) .|. 1

instance Record n => Record (Just n) where
  theInt _ = theInt (Proxy :: Proxy n)

type family Lookup (x :: k) (xs :: [k]) :: Nat where
  Lookup x (x ': xs) = Zero
  Lookup x (y ': ys) = Succ (Lookup x ys)
  Lookup x '[] = NotFound

type family Succ (x :: Nat) :: Nat where
  Succ Zero = SDNat Zero
  Succ (DNat n) = SDNat n
  Succ (SDNat n) = DNat (Succ n)
  Succ NotFound = NotFound
