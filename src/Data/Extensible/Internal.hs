{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Extensible.Internal (Position
  , runPosition
  , comparePosition
  , ord
  , Nav(..)
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
  , Check
  , Expecting
  , Missing
  , Ambiguous
  ) where
import Data.Type.Equality
import Data.Proxy
import Control.Applicative
import Control.Monad
import Unsafe.Coerce
import Data.Typeable
import Language.Haskell.TH

-- | Generates 'Position' for an ordinal (0-origin).
ord :: Int -> Q Exp
ord n = do
  let names = map mkName $ take (n + 1) $ concatMap (flip replicateM ['a'..'z']) [1..]
  let rest = mkName "any"
  let cons x xs = PromotedConsT `AppT` x `AppT` xs
  let t = foldr cons (VarT rest) (map VarT names)
  sigE (conE 'Position `appE` litE (IntegerL $ toInteger n))
    $ forallT (PlainTV rest : map PlainTV names) (pure [])
    $ conT ''Position `appT` pure t `appT` varT (names !! n)

-- | The position of @x@ in the type level set @xs@.
newtype Position (xs :: [k]) (x :: k) = Position Int deriving (Show, Eq, Ord, Typeable)

-- | Embodies a type equivalence to ensure that the 'Position' points the first element.
runPosition :: Position (y ': xs) x -> Either (x :~: y) (Position xs x)
runPosition (Position 0) = Left (unsafeCoerce Refl)
runPosition (Position n) = Right (Position (n - 1))
{-# INLINE runPosition #-}

comparePosition :: Position xs x -> Position xs y -> Maybe (x :~: y)
comparePosition (Position m) (Position n)
  | m == n = Just (unsafeCoerce Refl)
  | otherwise = Nothing
{-# INLINE comparePosition #-}

navigate :: Position xs x -> Nav xs x
navigate (Position 0) = unsafeCoerce Here
navigate (Position n) = let (m, r) = divMod (n - 1) 2 in case r of
  0 -> unsafeCoerce $ NavL $ Position m
  _ -> unsafeCoerce $ NavR $ Position m

data Nav xs x where
  Here :: Nav (x ': xs) x
  NavL :: Position (Half xs) x -> Nav (e ': xs) x
  NavR :: Position (Half (Tail xs)) x -> Nav (e ': xs) x

here :: Position (x ': xs) x
here = Position 0
{-# INLINE here #-}

navNext :: Position xs y -> Position (x ': xs) y
navNext (Position n) = Position (n + 1)
{-# INLINE navNext #-}

navL :: Position (Half xs) y -> Position (x ': xs) y
navL (Position x) = Position (x * 2 + 1)
{-# INLINE navL #-}

navR :: Position (Half (Tail xs)) y -> Position (x ': xs) y
navR (Position x) = Position (x * 2 + 2)
{-# INLINE navR #-}

-- | Unicode flipped alias for 'Member'
type x ∈ xs = Member xs x

-- | @Member x xs@ or @x ∈ xs@ indicates that @x@ is an element of @xs@.
class Member (xs :: [k]) (x :: k) where
  membership :: Position xs x

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
  membership = Position $ theInt (Proxy :: Proxy one)
  {-# INLINE membership #-}

type family Half (xs :: [k]) :: [k] where
  Half '[] = '[]
  Half (x ': y ': zs) = x ': Half zs
  Half (x ': '[]) = '[x]

type family Tail (xs :: [k]) :: [k] where
  Tail (x ': xs) = xs
  Tail '[] = '[]

data Nat = Zero | DNat Nat | SDNat Nat

retagD :: (Proxy n -> a) -> proxy (DNat n) -> a
retagD f _ = f Proxy
{-# INLINE retagD #-}

retagSD :: (Proxy n -> a) -> proxy (SDNat n) -> a
retagSD f _ = f Proxy
{-# INLINE retagSD #-}

class ToInt n where
  theInt :: proxy n -> Int

instance ToInt Zero where
  theInt _ = 0
  {-# INLINE theInt #-}

instance ToInt n => ToInt (DNat n) where
  theInt = (*2) <$> retagD theInt
  {-# INLINE theInt #-}

instance ToInt n => ToInt (SDNat n) where
  theInt = (+1) <$> (*2) <$> retagSD theInt
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

lemmaMerging :: p (Merge (Half xs) (Half (Tail xs))) -> p xs
lemmaMerging = unsafeCoerce

type family Map (f :: k -> k) (xs :: [k]) :: [k] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': xs ++ ys

type family Merge (xs :: [k]) (ys :: [k]) :: [k] where
  Merge (x ': xs) (y ': ys) = x ': y ': Merge xs ys
  Merge xs '[] = xs
  Merge '[] ys = ys
