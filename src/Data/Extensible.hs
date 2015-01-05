{-# LANGUAGE DataKinds, TypeOperators, PolyKinds, KindSignatures, GADTs, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This package defines an extensible type-indexed product type and a union type.
-- Both are determined from the type-level list of elements which has kind @[k]@
-- and a wrapper (k -> *).
-- We can define ADTs not only for plain values, but also parameterized ones.
--
-- >>> let t = K0 (42 :: Int) <:* K0 "foo" <:* K0 (Just "bar") <:* Nil
-- >>> t
-- K0 42 <:* K0 "foo" <:* K0 (Just "bar") <:* Nil
-- >>> :t t
-- t :: K0 :* '[Int, [Char], Maybe [Char]]
-- >>> pluck t :: Int
-- 42
-----------------------------------------------------------------------------
module Data.Extensible (
  -- * Lookup
    Position
  , runPosition
  , (∈)()
  , Member(..)
  -- * Product
  , (:*)(..)
  , (<:*)
  , unconsP
  , hoistP
  , outP
  , sector
  , sectorAt
  , Generate(..)
  -- * Sum
  , (:|)(..)
  , (<:|)
  , exhaust
  , inS
  , picked
  -- * Inclusion/Permutation
  , Include(..)
  -- * Pattern match
  , Match(..)
  , match
  , mapMatch
  -- * Monomorphic
  , K0(..)
  , (<%)
  , pluck
  , bury
  , (<%|)
  , record
  , (<?%)
  -- * Parameterized
  , K1(..)
  , Union(..)
  , liftU
  , (<?!)
  -- * Improved Union
  , Fuse(..)
  , meltdown
  , League(..)
  , liftL
  , (<?~)
  ) where
import Unsafe.Coerce
import Data.Bits
import Data.Typeable
import Control.Applicative

-- | The extensible product type
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

unconsP :: forall h x xs. h :* (x ': xs) -> (h x, h :* xs)
unconsP (Tree a Nil _) = (a, lemmaHalfEmpty (Proxy :: Proxy xs) Nil)
unconsP (Tree a bd c) = (a, let (b, d) = unconsP (unsafeCoerce bd) in unsafeCoerce $ Tree b (unsafeCoerce c) d)

lemmaHalfEmpty :: (Half xs ~ '[]) => Proxy xs -> p '[] -> p xs
lemmaHalfEmpty _ = unsafeCoerce

lemmaHalfTail :: Proxy xs -> p (x ': Half (Tail xs)) -> p (Half (x ': xs))
lemmaHalfTail _ = unsafeCoerce

-- | /O(log n)/ Add an element to a product.
(<:*) :: forall h x xs. h x -> h :* xs -> h :* (x ': xs)
a <:* Tree b c d = Tree a (lemmaHalfTail (Proxy :: Proxy (Tail xs)) $! b <:* d) c
a <:* Nil = Tree a Nil Nil
infixr 5 <:*

hoistP :: (forall x. g x -> h x) -> g :* xs -> h :* xs
hoistP t (Tree h a b) = Tree (t h) (hoistP t a) (hoistP t b)
hoistP _ Nil = Nil

-- | /O(log n)/ Pick a specific element.
outP :: forall h x xs. (x ∈ xs) => h :* xs -> h x
outP = view $ sectorAt (position :: Position x xs)
{-# INLINE outP #-}

-- | /O(log n)/ A lens for a specific element.
sector :: forall h x xs f. (Functor f, x ∈ xs) => (h x -> f (h x)) -> h :* xs -> f (h :* xs)
sector = sectorAt (position :: Position x xs)
{-# INLINE sector #-}

view :: ((a -> Const a a) -> (s -> Const a s)) -> s -> a
view l = unsafeCoerce (l Const)
{-# INLINE view #-}

-- | /O(log n)/
sectorAt :: forall h x xs f. (Functor f) => Position x xs -> (h x -> f (h x)) -> h :* xs -> f (h :* xs)
sectorAt pos0 f = go pos0 where
  go :: forall t. Position x t -> h :* t -> f (h :* t)
  go pos (Tree h a b) = case runPosition pos of
    Left Refl -> fmap (\h' -> Tree h' a b) (f h)
    Right (Position m) -> case m .&. 1 of
      0 -> fmap (\a' -> Tree h a' b)
        $ go (Position (shiftR m 1) :: Position x (Half (Tail t))) a
      _ -> fmap (\b' -> Tree h a b')
        $ go (Position (shiftR m 1) :: Position x (Half (Tail (Tail t)))) b
  go _ Nil = error "Impossible"
{-# INLINE sectorAt #-}

-- | /O(log n)/ lift a value.
inS :: (x ∈ xs) => h x -> h :| xs
inS = UnionAt position
{-# INLINE inS #-}

picked :: forall f h x xs. (x ∈ xs, Applicative f) => (h x -> f (h x)) -> h :| xs -> f (h :| xs)
picked f u@(UnionAt (Position n) h)
  | n == m = fmap (UnionAt (Position n)) $ f (unsafeCoerce h)
  | otherwise = pure u
  where
    Position m = position :: Position x xs

runPosition :: Position x (y ': xs) -> Either (x :~: y) (Position x xs)
runPosition (Position 0) = Left (unsafeCoerce Refl)
runPosition (Position n) = Right (Position (n - 1))
{-# INLINE runPosition #-}

-- | /O(1)/ Naive pattern match
(<:|) :: (h x -> r) -> (h :| xs -> r) -> h :| (x ': xs) -> r
(<:|) r c = \(UnionAt pos h) -> case runPosition pos of
  Left Refl -> r h
  Right pos' -> c (UnionAt pos' h)
infixr 1 <:|
{-# INLINE (<:|) #-}

exhaust :: h :| '[] -> r
exhaust _ = error "Impossible"

-- | The extensible sum type
data (h :: k -> *) :| (s :: [k]) where
  UnionAt :: Position x xs -> h x -> h :| xs

instance Show (h :| '[]) where
  show = exhaust

instance (Show (h x), Show (h :| xs)) => Show (h :| (x ': xs)) where
  showsPrec d = (\h -> showParen (d > 10) $ showString "inS " . showsPrec 11 h)
    <:| showsPrec d

class Generate (xs :: [k]) where
  generate :: (forall x. Position x xs -> h x) -> h :* xs

instance Generate '[] where
  generate _ = Nil
  {-# INLINE generate #-}
instance Generate xs => Generate (x ': xs) where
  generate f = f (Position 0) <:* generate (f . succPos) where
    succPos (Position n) = Position (n + 1)
    {-# INLINE succPos #-}
  {-# INLINE generate #-}

newtype K0 a = K0 { getK0 :: a } deriving (Eq, Ord, Read, Typeable)

-- | /O(log n)/ Add a plain value to a product.
(<%) :: x -> K0 :* xs -> K0 :* (x ': xs)
(<%) = unsafeCoerce (<:*)
{-# INLINE (<%) #-}
infixr 5 <%

pluck :: (x ∈ xs) => K0 :* xs -> x
pluck = getK0 . outP

bury :: (x ∈ xs) => x -> K0 :| xs
bury = inS . K0

(<%|) :: (x -> r) -> (K0 :| xs -> r) -> K0 :| (x ': xs) -> r
(<%|) = unsafeCoerce (<:|)

instance Show a => Show (K0 a) where
  showsPrec d (K0 a) = showParen (d > 10) $ showString "K0 " . showsPrec 11 a

-- | /O(log n)/ A lens for a plain value in a product.
record :: forall f x xs. (x ∈ xs, Functor f) => (x -> f x) -> (K0 :* xs -> f (K0 :* xs))
record = unsafeCoerce (sector :: (K0 x -> f (K0 x)) -> (K0 :* xs -> f (K0 :* xs)))
{-# INLINE record #-}

newtype K1 a f = K1 { getK1 :: f a } deriving (Eq, Ord, Read, Typeable)

instance Show (f a) => Show (K1 a f) where
  showsPrec d (K1 a) = showParen (d > 10) $ showString "K1 " . showsPrec 11 a

newtype Match h a x = Match { runMatch :: h x -> a }

mapMatch :: (a -> b) -> Match h a x -> Match h b x
mapMatch f (Match g) = Match (f . g)
{-# INLINE mapMatch #-}

-- | /O(log n)/ Perform pattern match.
match :: Match h a :* xs -> h :| xs -> a
match p (UnionAt pos h) = runMatch (view (sectorAt pos) p) h
{-# INLINE match #-}

(<?%) :: (x -> a) -> Match K0 a :* xs -> Match K0 a :* (x ': xs)
(<?%) = unsafeCoerce (<:*)
infixr 1 <?%

(<?!) :: (f x -> a) -> Match (K1 x) a :* xs -> Match (K1 x) a :* (f ': fs)
(<?!) = unsafeCoerce (<:*)
infixr 1 <?!

newtype Union fs a = Union { getUnion :: K1 a :| fs }

liftU :: (f ∈ fs) => f a -> Union fs a
liftU = Union . inS . K1
{-# INLINE liftU #-}

instance Show (Union '[] a) where
  show (Union u) = exhaust u

instance (Show (f a), Show (Union fs a)) => Show (Union (f ': fs) a) where
  showsPrec d (Union u) = (\(K1 f) -> showParen (d > 10) $ showString "liftU " . showsPrec 11 f)
    <:| showsPrec d . Union
    $ u

instance Functor (Union '[]) where
  fmap _ = exhaust . getUnion

-- | slow fmap
instance (Functor f, Functor (Union fs)) => Functor (Union (f ': fs)) where
  fmap f (Union (UnionAt pos@(Position n) (K1 h))) = case runPosition pos of
    Left Refl -> Union $ UnionAt pos $ K1 (fmap f h)
    Right pos' -> case fmap f (Union (UnionAt pos' (K1 h))) of
      Union (UnionAt _ h') -> Union (UnionAt (Position n) h')

-- | Better representation of a union of functors.
newtype League fs a = League { getLeague :: Fuse a :| fs }

-- | /O(log n)/ Embed a functor along with 'fmap'.
liftL :: (Functor f, f ∈ fs) => f a -> League fs a
liftL f = League $ inS $ Fuse $ \g -> fmap g f
{-# INLINE liftL #-}

-- | Flipped <http://hackage.haskell.org/package/kan-extensions-4.1.0.1/docs/Data-Functor-Yoneda.html Yoneda>
newtype Fuse a f = Fuse { getFuse :: forall b. (a -> b) -> f b }

meltdown :: Fuse a f -> f a
meltdown (Fuse f) = f id
{-# INLINE meltdown #-}

mapFuse :: (a -> b) -> Fuse a f -> Fuse b f
mapFuse f (Fuse g) = Fuse (\h -> g (h . f))
{-# INLINE mapFuse #-}

-- | fast fmap
instance Functor (League fs) where
  fmap f (League (UnionAt pos s)) = League (UnionAt pos (mapFuse f s))
  {-# INLINE fmap #-}

(<?~) :: (f x -> a) -> Match (Fuse x) a :* fs -> Match (Fuse x) a :* (f ': fs)
(<?~) f = (<:*) (Match (f . meltdown))
infixr 1 <?~

---------------------------------------------------------------------

newtype Position (x :: k) (xs :: [k]) = Position Int deriving (Show, Eq, Ord)

type (∈) = Member

class Member (x :: k) (xs :: [k]) where
  position :: Position x xs

instance Record (Lookup x xs) => Member x xs where
  position = Position $ theInt (Proxy :: Proxy (Lookup x xs))
  {-# INLINE position #-}

class Include (xs :: [k]) (ys :: [k]) where
  -- | /O(m log n)/ Select some elements.
  shrink :: h :* ys -> h :* xs
  -- | /O(m log n)/ Embed to a larger union.
  spread :: h :| xs -> h :| ys

instance Include '[] xs where
  shrink _ = Nil
  spread = exhaust

instance (x ∈ ys, Include xs ys) => Include (x ': xs) ys where
  shrink ys = outP ys <:* shrink ys
  spread xs = inS <:| spread $ xs

type family Half (xs :: [k]) :: [k] where
  Half '[] = '[]
  Half (x ': y ': zs) = x ': zs
  Half (x ': '[]) = x ': '[]

type family Tail (xs :: [k]) :: [k] where
  Tail (x ': xs) = xs
  Tail '[] = '[]

data Nat = Zero | DNat Nat | SDNat Nat | NotFound

retagD :: (Proxy n -> a) -> proxy (DNat n) -> a
retagD f _ = f Proxy
{-# INLINE retagD #-}

retagSD :: (Proxy n -> a) -> proxy (SDNat n) -> a
retagSD f _ = f Proxy
{-# INLINE retagSD #-}

class Record n where
  theInt :: Proxy n -> Int

instance Record Zero where
  theInt _ = 0
  {-# INLINE theInt #-}

instance Record n => Record (DNat n) where
  theInt = (\n -> n + n) <$> retagD theInt
  {-# INLINE theInt #-}

instance Record n => Record (SDNat n) where
  theInt = (\n -> n + n + 1) <$> retagSD theInt
  {-# INLINE theInt #-}

type family Lookup (x :: k) (xs :: [k]) :: Nat where
  Lookup x (x ': xs) = Zero
  Lookup x (y ': ys) = Succ (Lookup x ys)
  Lookup x '[] = NotFound

type family Succ (x :: Nat) :: Nat where
  Succ Zero = SDNat Zero
  Succ (DNat n) = SDNat n
  Succ (SDNat n) = DNat (Succ n)
  Succ NotFound = NotFound
