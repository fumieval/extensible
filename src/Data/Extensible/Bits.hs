{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif
-----------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Bits
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Bit-packed records
-----------------------------------------------------------------------
module Data.Extensible.Bits (BitProd(..)
  , FromBits(..)
  , TotalBits
  , BitFields
  , blookup
  , bupdate
  , toBitProd
  , fromBitProd
  , BitRecordOf
  , BitRecord) where

import Control.Applicative
import Control.Comonad
import Data.Bits
import Data.Extensible.Class
import Data.Extensible.Dictionary
import Data.Extensible.Product
import Data.Extensible.Field
import Data.Functor.Identity
import Data.Hashable
import Data.Ix
import Data.Kind (Type)
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Proxy
import Data.Word
import Data.Int
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GHC.TypeLits

-- | Bit-vector product. It has similar interface as @(:*)@ but fields are packed into @r@.
newtype BitProd r (xs :: [k]) (h :: k -> Type) = BitProd { unBitProd :: r }
  deriving (Eq, Ord, Enum, Bounded, Ix, Generic, Hashable, Storable)

instance (Forall (Instance1 Show h) xs, BitFields r xs h) => Show (BitProd r xs h) where
  showsPrec d x = showParen (d > 10)
    $ showString "toBitProd " . showsPrec 11 (fromBitProd x)

-- | Total 'BitWidth'
type family TotalBits h xs where
  TotalBits h '[] = 0
  TotalBits h (x ': xs) = BitWidth (h x) + TotalBits h xs

-- | Conversion between a value and a bit representation.
--
-- Instances of `FromBits` must satisfy the following laws:
--
-- > fromBits (x `shiftL` W .|. toBits a) ≡ a
-- > toBits a `shiftR` W == zeroBits
--
-- where W is the 'BitWidth'.
class (Bits r, KnownNat (BitWidth a)) => FromBits r a where
  type BitWidth a :: Nat
  fromBits :: r -> a
  toBits :: a -> r

instance Bits r => FromBits r () where
  type BitWidth () = 0
  fromBits _ = ()
  toBits _ = zeroBits

instance Bits r => FromBits r (Proxy a) where
  type BitWidth (Proxy a) = 0
  fromBits _ = Proxy
  toBits _ = zeroBits

instance FromBits Word64 Word64 where
  type BitWidth Word64 = 64
  fromBits = id
  toBits = id

instance FromBits Word64 Bool where
  type BitWidth Bool = 1
  fromBits = flip testBit 0
  toBits False = 0
  toBits True = 1

instance FromBits Word64 Word8 where
  type BitWidth Word8 = 8
  fromBits = fromIntegral
  toBits = fromIntegral

instance FromBits Word64 Word16 where
  type BitWidth Word16 = 16
  fromBits = fromIntegral
  toBits = fromIntegral

instance FromBits Word64 Word32 where
  type BitWidth Word32 = 32
  fromBits = fromIntegral
  toBits = fromIntegral

instance FromBits Word64 Int8 where
  type BitWidth Int8 = 8
  fromBits = fromIntegral
  toBits = fromIntegral . (fromIntegral :: Int8 -> Word8)

instance FromBits Word64 Int16 where
  type BitWidth Int16 = 16
  fromBits = fromIntegral
  toBits = fromIntegral . (fromIntegral :: Int16 -> Word16)

instance FromBits Word64 Int32 where
  type BitWidth Int32 = 32
  fromBits = fromIntegral
  toBits = fromIntegral . (fromIntegral :: Int32 -> Word32)

instance FromBits r a => FromBits r (Identity a) where
  type BitWidth (Identity a) = BitWidth a
  fromBits = Identity . fromBits
  toBits = toBits . runIdentity

instance (FromBits r a, FromBits r b, n ~ (BitWidth a + BitWidth b), n <= BitWidth r, KnownNat n) => FromBits r (a, b) where
  type BitWidth (a, b) = BitWidth a + BitWidth b
  fromBits r = (fromBits (unsafeShiftR r width), fromBits r) where
    width = fromInteger $ natVal (Proxy :: Proxy (BitWidth b))
  toBits (a, b) = unsafeShiftL (toBits a) width .|. toBits b where
    width = fromInteger $ natVal (Proxy :: Proxy (BitWidth b))

instance FromBits r a => FromBits r (Const a b) where
  type BitWidth (Const a b) = BitWidth a
  fromBits = Const . fromBits
  toBits = toBits . getConst

instance (Bits r, KnownNat (BitWidth (h (TargetOf x))), FromBits r (h (TargetOf x))) => FromBits r (Field h x) where
  type BitWidth (Field h x) = BitWidth (h (TargetOf x))
  fromBits = Field . fromBits
  toBits = toBits . getField

instance (Bits r, KnownNat (TotalBits h xs)) => FromBits r (BitProd r xs h) where
  type BitWidth (BitProd r xs h) = TotalBits h xs
  fromBits = BitProd
  toBits = unBitProd

-- | Fields are instances of 'FromBits' and fit in the representation.
type BitFields r xs h = (FromBits r r
  , TotalBits h xs <= BitWidth r
  , Forall (Instance1 (FromBits r) h) xs)

-- | Convert a normal extensible record into a bit record.
toBitProd :: forall r xs h. BitFields r xs h => xs :& h -> BitProd r xs h
toBitProd p = hfoldrWithIndexFor (Proxy :: Proxy (Instance1 (FromBits r) h))
  (\i v f r -> f $! bupdate i r v) id p (BitProd zeroBits)
{-# INLINE toBitProd #-}

-- | Convert a normal extensible record into a bit record.
fromBitProd :: forall r xs h. BitFields r xs h => BitProd r xs h -> xs :& h
fromBitProd p = htabulateFor (Proxy :: Proxy (Instance1 (FromBits r) h))
  $ flip blookup p
{-# INLINE fromBitProd #-}

-- | 'hlookup' for 'BitProd'
blookup :: forall x r xs h.
  (BitFields r xs h, FromBits r (h x))
  => Membership xs x -> BitProd r xs h -> h x
blookup i (BitProd r) = fromBits $ unsafeShiftR r
  $ bitOffsetAt (Proxy :: Proxy r) (Proxy :: Proxy h) (Proxy :: Proxy xs)
  $ getMemberId i
{-# INLINE blookup #-}

-- | Update a field of a 'BitProd'.
bupdate :: forall x r xs h.
  (BitFields r xs h, FromBits r (h x))
  => Membership xs x -> BitProd r xs h -> h x -> BitProd r xs h
bupdate i (BitProd r) a = BitProd $ r .&. mask
  .|. unsafeShiftL (toBits a) offset
  where
    mask = unsafeShiftL (complement zeroBits) width `rotateL` offset
    width = fromInteger $ natVal (Proxy :: Proxy (BitWidth (h x)))
    offset = bitOffsetAt (Proxy :: Proxy r) (Proxy :: Proxy h) (Proxy :: Proxy xs) $ getMemberId i
{-# INLINE bupdate #-}

bitOffsetAt :: forall k r h xs. Forall (Instance1 (FromBits r) h) xs
  => Proxy (r :: Type) -> Proxy (h :: k -> Type) -> Proxy (xs :: [k]) -> Int -> Int
bitOffsetAt _ ph _ = henumerateFor
  (Proxy :: Proxy (Instance1 (FromBits r) h))
  (Proxy :: Proxy xs)
  (\m r o i -> if i == 0
    then o
    else r (fromInteger (natVal (proxyBitWidth ph m)) + o) (i - 1))
  (error "Impossible") 0
{-# INLINE bitOffsetAt #-}

proxyBitWidth :: Proxy h -> proxy x -> Proxy (BitWidth (h x))
proxyBitWidth _ _ = Proxy

-- | Bit-packed record
type BitRecordOf r h xs = BitProd r xs (Field h)

-- | Bit-packed record
type BitRecord r xs = BitRecordOf r Identity xs

instance (Corepresentable p, Comonad (Corep p), Functor f) => Extensible f p (BitProd r) where
  type ExtensibleConstr (BitProd r) xs h x
    = (BitFields r xs h, FromBits r (h x))
  pieceAt i pafb = cotabulate $ \ws -> bupdate i (extract ws) <$> cosieve pafb (blookup i <$> ws)
  {-# INLINE pieceAt #-}
