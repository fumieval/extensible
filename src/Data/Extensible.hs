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
-- Both are determined from the type-level list @[k]@
-- and a wrapper @k -> *@.
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
  -- * Product
  (:*)(..)
  , (<:*)
  , unconsP
  , hoistP
  , zipWithP
  , zipWith3P
  , foldP
  , outP
  , sector
  , sectorAt
  , Generate(..)
  , Forall(..)
  -- * Sum
  , (:|)(..)
  , (<:|)
  , exhaust
  , inS
  , picked
  -- * Inclusion/Permutation
  , Include(..)
  , (⊆)()
  , shrink
  , spread
  -- * Pattern match
  , Match(..)
  , match
  , mapMatch
  , caseOf
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
  , mapFuse
  , League(..)
  , liftL
  , (<?~)
  -- * Lookup
  , Position
  , runPosition
  , Possession(..)
  , (∈)()
  , Member(..)
  ) where
import Unsafe.Coerce
import Data.Bits
import Data.Typeable
import Control.Applicative
import Data.Monoid

-- | The extensible product type
data (h :: k -> *) :* (s :: [k]) where
  Nil :: h :* '[]
  Tree :: h x
    -> h :* Half xs
    -> h :* Half (Tail xs)
    -> h :* (x ': xs)

deriving instance Typeable (:*)

instance Show (h :* '[]) where
  show Nil = "Nil"

instance (Show (h :* xs), Show (h x)) => Show (h :* (x ': xs)) where
  showsPrec d (huncons -> (x, xs)) = showParen (d > 10) $
     showsPrec 6 x
    . showString " <:* "
    . showsPrec 6 xs

htail :: h :* (x ': xs) -> h :* xs
htail (Tree _ a@(Tree h _ _) b) = unsafeCoerce (Tree h) b (htail a)
htail (Tree _ Nil _) = unsafeCoerce Nil

-- | Split a product to the head and the tail.
huncons :: forall h x xs. h :* (x ': xs) -> (h x, h :* xs)
huncons t@(Tree a _ _) = (a, htail t)

-- GHC can't prove this
lemmaHalfTail :: Proxy xs -> p (x ': Half (Tail xs)) -> p (Half (x ': xs))
lemmaHalfTail _ = unsafeCoerce

-- | /O(log n)/ Add an element to a product.
(<:*) :: forall h x xs. h x -> h :* xs -> h :* (x ': xs)
a <:* Tree b c d = Tree a (lemmaHalfTail (Proxy :: Proxy (Tail xs)) $! b <:* d) c
a <:* Nil = Tree a Nil Nil
infixr 5 <:*

-- | Transform every elements in a union, preserving the order.
hhoist :: (forall x. g x -> h x) -> g :* xs -> h :* xs
hhoist t (Tree h a b) = Tree (t h) (hhoist t a) (hhoist t b)
hhoist _ Nil = Nil

hzipWith :: (forall x. f x -> g x -> h x) -> f :* xs -> g :* xs -> h :* xs
hzipWith t (Tree f a b) (Tree g c d) = Tree (t f g) (hzipWith t a c) (hzipWith t b d)
hzipWith _ Nil _ = Nil
hzipWith _ _ Nil = Nil

hzipWith3 :: (forall x. f x -> g x -> h x -> i x) -> f :* xs -> g :* xs -> h :* xs -> i :* xs
hzipWith3 t (Tree f a b) (Tree g c d) (Tree h e f') = Tree (t f g h) (hzipWith3 t a c e) (hzipWith3 t b d f')
hzipWith3 _ Nil _ _ = Nil
hzipWith3 _ _ Nil _ = Nil
hzipWith3 _ _ _ Nil = Nil

hfold :: Monoid a => (forall x. h x -> a) -> h :* xs -> a
hfold f t@(Tree h a b) = f h <> hfold f a <> hfold f b
hfold _ Nil = mempty

htraverse :: Applicative f => (forall x. h x -> f (h x)) -> h :* xs -> f (h :* xs)
htraverse f (Tree h a b) = Tree <$> f h <*> htraverse f a <*> htraverse f b
htraverse _ Nil = pure Nil

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

-- | /O(log n)/ A lens for a value in a known position.
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

-- | Given a function that maps types to values, we can "collect" entities all you want.
class Generate (xs :: [k]) where
  generate :: (forall x. Position x xs -> h x) -> h :* xs

instance Generate '[] where
  generate _ = Nil
  {-# INLINE generate #-}

lw :: Position a b -> Position c d
lw (Position x) = Position (x * 2 + 1)

rw :: Position a b -> Position c d
rw (Position x) = Position ((x + 1) * 2)

instance (Generate (Half xs), Generate (Half (Tail xs))) => Generate (x ': xs) where
  generate f = Tree (f (Position 0)) (generate (f . lw)) (generate (f . rw)) where
  {-# INLINE generate #-}

-- | Given a function that maps types to values, we can "collect" entities all you want.
class Forall c (xs :: [k]) where
  generateFor :: Proxy c -> (forall x. c x => Position x xs -> h x) -> h :* xs

instance Forall c '[] where
  generateFor _ _ = Nil
  {-# INLINE generateFor #-}

instance (c x, Forall c (Half xs), Forall c (Half (Tail xs))) => Forall c (x ': xs) where
  generateFor proxy f = Tree (f (Position 0)) (generateFor proxy (f . lw)) (generateFor proxy (f . rw)) where
  {-# INLINE generateFor #-}

class c (h x) => Comp c h x
instance c (h x) => Comp c h x

instance Forall (Comp Eq h) xs => Eq (h :* xs) where
  (==) = (aggr.) . hzipWith3 (\(Possession pos) -> (Const' .) . unwrapEq (view (sectorAt pos) dic))
    (generateFor (Proxy :: Proxy (Comp Eq h)) Possession) where
      dic = generateFor (Proxy :: Proxy (Comp Eq h)) $ const $ WrapEq (==)
      aggr = getAll . hfold (All . getConst')

newtype WrapEq h x = WrapEq { unwrapEq :: h x -> h x -> Bool }

-- | The extensible sum type
data (h :: k -> *) :| (s :: [k]) where
  UnionAt :: Position x xs -> h x -> h :| xs
deriving instance Typeable (:|)

-- | Poly-kinded Const
newtype Const' a x = Const' { getConst' :: a } deriving Show

instance Show (h :| '[]) where
  show = exhaust

instance (Show (h x), Show (h :| xs)) => Show (h :| (x ': xs)) where
  showsPrec d = (\h -> showParen (d > 10) $ showString "inS " . showsPrec 11 h)
    <:| showsPrec d

-- | /O(1)/ Naive pattern match
(<:|) :: (h x -> r) -> (h :| xs -> r) -> h :| (x ': xs) -> r
(<:|) r c = \(UnionAt pos h) -> case runPosition pos of
  Left Refl -> r h
  Right pos' -> c (UnionAt pos' h)
infixr 1 <:|
{-# INLINE (<:|) #-}

-- | /O(log n)/ lift a value.
inS :: (x ∈ xs) => h x -> h :| xs
inS = UnionAt position
{-# INLINE inS #-}

-- | A traversal that tries to point a specific element.
picked :: forall f h x xs. (x ∈ xs, Applicative f) => (h x -> f (h x)) -> h :| xs -> f (h :| xs)
picked f u@(UnionAt (Position n) h)
  | n == m = fmap (UnionAt (Position n)) $ f (unsafeCoerce h)
  | otherwise = pure u
  where
    Position m = position :: Position x xs

-- | There is no empty union.
exhaust :: h :| '[] -> r
exhaust _ = error "Impossible"

-------------------------------------------------------------

-- | Just a value.
newtype K0 a = K0 { getK0 :: a } deriving (Eq, Ord, Read, Typeable)

-- | /O(log n)/ Add a plain value to a product.
(<%) :: x -> K0 :* xs -> K0 :* (x ': xs)
(<%) = unsafeCoerce (<:*)
{-# INLINE (<%) #-}
infixr 5 <%

-- | Extract a plain value.
pluck :: (x ∈ xs) => K0 :* xs -> x
pluck = getK0 . outP

-- | Embed a plain value.
bury :: (x ∈ xs) => x -> K0 :| xs
bury = inS . K0

-- | Naive pattern matching for a plain value.
(<%|) :: (x -> r) -> (K0 :| xs -> r) -> K0 :| (x ': xs) -> r
(<%|) = unsafeCoerce (<:|)

instance Show a => Show (K0 a) where
  showsPrec d (K0 a) = showParen (d > 10) $ showString "K0 " . showsPrec 11 a

-- | /O(log n)/ A lens for a plain value in a product.
record :: (x ∈ xs, Functor f) => (x -> f x) -> (K0 :* xs -> f (K0 :* xs))
record f = sector $ unsafeCoerce f `asTypeOf` (fmap K0 . f . getK0)
{-# INLINE record #-}

-- | Wrap a type that has a kind @* -> *@.
newtype K1 a f = K1 { getK1 :: f a } deriving (Eq, Ord, Read, Typeable)

instance Show (f a) => Show (K1 a f) where
  showsPrec d (K1 a) = showParen (d > 10) $ showString "K1 " . showsPrec 11 a

-- | Turn a wrapper type into one clause that returns @a@.
newtype Match h a x = Match { runMatch :: h x -> a }

-- | Applies a function to the result of 'Match'.
mapMatch :: (a -> b) -> Match h a x -> Match h b x
mapMatch f (Match g) = Match (f . g)
{-# INLINE mapMatch #-}

-- | /O(log n)/ Perform pattern matching.
match :: Match h a :* xs -> h :| xs -> a
match p (UnionAt pos h) = runMatch (view (sectorAt pos) p) h
{-# INLINE match #-}

-- | Flipped `match`
caseOf :: h :| xs -> Match h a :* xs -> a
caseOf = flip match
{-# INLINE caseOf #-}
infix 0 `caseOf`

-- | Prepend a clause for a plain value.
(<?%) :: (x -> a) -> Match K0 a :* xs -> Match K0 a :* (x ': xs)
(<?%) = unsafeCoerce (<:*)
infixr 1 <?%

-- | Prepend a clause for a parameterized value.
(<?!) :: (f x -> a) -> Match (K1 x) a :* xs -> Match (K1 x) a :* (f ': fs)
(<?!) = unsafeCoerce (<:*)
infixr 1 <?!

-- | A wrapper for @'K1' a ':|'' fs@ for having a kind @* -> *@.
newtype Union fs a = Union { getUnion :: K1 a :| fs } deriving Typeable

-- | /O(log n)/ Lift a value.
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

-- | A much more efficient representation for 'Union' of 'Functor's.
newtype League fs a = League { getLeague :: Fuse a :| fs } deriving Typeable

-- | /O(log n)/ Embed a functor.
liftL :: (Functor f, f ∈ fs) => f a -> League fs a
liftL f = League $ inS $ Fuse $ \g -> fmap g f
{-# INLINE liftL #-}

-- | Flipped <http://hackage.haskell.org/package/kan-extensions-4.1.0.1/docs/Data-Functor-Yoneda.html Yoneda>
newtype Fuse a f = Fuse { getFuse :: forall b. (a -> b) -> f b }

-- | Fuse 'Fuse' to retract a substantial functor.
meltdown :: Fuse a f -> f a
meltdown (Fuse f) = f id
{-# INLINE meltdown #-}

-- | 'fmap' for the content.
mapFuse :: (a -> b) -> Fuse a f -> Fuse b f
mapFuse f (Fuse g) = Fuse (\h -> g (h . f))
{-# INLINE mapFuse #-}

-- | fast fmap
instance Functor (League fs) where
  fmap f (League (UnionAt pos s)) = League (UnionAt pos (mapFuse f s))
  {-# INLINE fmap #-}

-- | Prepend a clause for @'Match' ('Fuse' x)@ as well as ('<?!').
(<?~) :: (f x -> a) -> Match (Fuse x) a :* fs -> Match (Fuse x) a :* (f ': fs)
(<?~) f = (<:*) (Match (f . meltdown))
infixr 1 <?~

---------------------------------------------------------------------

-- | The position of @x@ in the type level set @xs@.
newtype Position (x :: k) (xs :: [k]) = Position Int deriving (Show, Eq, Ord)
-- should it be flipped?

-- | Embodies a type equivalence to ensure that the 'Position' points the first element.
runPosition :: Position x (y ': xs) -> Either (x :~: y) (Position x xs)
runPosition (Position 0) = Left (unsafeCoerce Refl)
runPosition (Position n) = Right (Position (n - 1))
{-# INLINE runPosition #-}

-- | Unicode alias for 'Member'
type (∈) = Member

-- | @Member x xs@ or @x ∈ xs@ indicates that @x@ is an element of @xs@.
class Member (x :: k) (xs :: [k]) where
  position :: Position x xs

instance Record (Lookup x xs) => Member x xs where
  position = Position $ theInt (Proxy :: Proxy (Lookup x xs))
  {-# INLINE position #-}

-- | Flipped 'Position'
newtype Possession xs x = Possession { getPossession :: Position x xs } deriving (Show, Eq, Ord, Typeable)

-- | ys contains xs
class Include (xs :: [k]) (ys :: [k]) where
  possession :: Possession ys :* xs

-- | Unicode alias for 'Include'
type (⊆) = Include

-- | /O(m log n)/ Select some elements.
shrink :: (xs ⊆ ys) => h :* ys -> h :* xs
shrink = let !p = possession in \h -> hhoist (\(Possession pos) -> sectorAt pos `view` h) p

-- | /O(m log n)/ Embed to a larger union.
spread :: (xs ⊆ ys) => h :| xs -> h :| ys
spread = let !p = possession in \(UnionAt pos h) -> getPossession (sectorAt pos `view` p) `UnionAt` h

instance Include '[] xs where
  possession = Nil
instance (x ∈ ys, xs ⊆ ys) => Include (x ': xs) ys where
  possession = Possession position <:* possession

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
  theInt = (*2) <$> retagD theInt
  {-# INLINE theInt #-}

instance Record n => Record (SDNat n) where
  theInt = (+1) <$> (*2) <$> retagSD theInt
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
