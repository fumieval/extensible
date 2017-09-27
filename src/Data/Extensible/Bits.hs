{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

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

import Control.Comonad
import Data.Bits
import Data.Extensible.Class
import Data.Extensible.Dictionary
import Data.Extensible.Product
import Data.Extensible.Internal (getMemberId)
import Data.Extensible.Field
import Data.Functor.Identity
import Data.Ix
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Proxy
import Data.Word
import Data.Int
import GHC.Generics (Generic)
import GHC.TypeLits

-- | Bit-vector product. It has similar interface as @(:*)@ but fields are packed into @r@.
newtype BitProd r (h :: k -> *) (xs :: [k]) = BitProd { unBitProd :: r }
  deriving (Eq, Ord, Enum, Bounded, Ix, Generic)

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

instance (Bits r, FromBits r (h (AssocValue x))) => FromBits r (Field h x) where
  type BitWidth (Field h x) = BitWidth (h (AssocValue x))
  fromBits = Field . fromBits
  toBits = toBits . getField

instance (Bits r, KnownNat (TotalBits h xs)) => FromBits r (BitProd r h xs) where
  type BitWidth (BitProd r h xs) = TotalBits h xs
  fromBits = BitProd
  toBits = unBitProd

-- | Fields are instances of 'FromBits' and fit in the representation.
type BitFields r h xs = (FromBits r r
  , TotalBits h xs <= BitWidth r
  , Forall (Instance1 (FromBits r) h) xs)

-- | Convert a normal extensible record into a bit record.
toBitProd :: forall r h xs. BitFields r h xs => h :* xs -> BitProd r h xs
toBitProd p = hfoldrWithIndexFor (Proxy :: Proxy (Instance1 (FromBits r) h))
  (\i v f r -> f $! bupdate i r v) id p (BitProd zeroBits)

-- | Convert a normal extensible record into a bit record.
fromBitProd :: forall r h xs. BitFields r h xs => BitProd r h xs -> h :* xs
fromBitProd p = htabulateFor (Proxy :: Proxy (Instance1 (FromBits r) h))
  $ flip blookup p

-- | 'hlookup' for 'BitProd'
blookup :: forall x r h xs.
  (BitFields r h xs, FromBits r (h x))
  => Membership xs x -> BitProd r h xs -> h x
blookup i (BitProd r) = fromBits $ unsafeShiftR r
  $ bitOffsetAt (Proxy :: Proxy r) (Proxy :: Proxy h) (Proxy :: Proxy xs)
  $ getMemberId i
{-# INLINE blookup #-}

-- | Update a field of a 'BitProd'.
bupdate :: forall x r h xs.
  (BitFields r h xs, FromBits r (h x))
  => Membership xs x -> BitProd r h xs -> h x -> BitProd r h xs
bupdate i (BitProd r) a = BitProd $ r .&. mask
  .|. unsafeShiftL (toBits a) offset
  where
    mask = unsafeShiftL (complement zeroBits) width `rotateL` offset
    width = fromInteger $ natVal (Proxy :: Proxy (BitWidth (h x)))
    offset = bitOffsetAt (Proxy :: Proxy r) (Proxy :: Proxy h) (Proxy :: Proxy xs) $ getMemberId i
{-# INLINE bupdate #-}

bitOffsetAt :: forall r h xs. Forall (Instance1 (FromBits r) h) xs
  => Proxy (r :: *) -> Proxy (h :: k -> *) -> Proxy (xs :: [k]) -> Int -> Int
bitOffsetAt _ ph _ = henumerateFor
  (Proxy :: Proxy (Instance1 (FromBits r) h))
  (Proxy :: Proxy xs)
  (\m r o i -> if i == 0
    then o
    else r (fromInteger (natVal (proxyBitWidth ph m)) + o) (i - 1))
  (error "Impossible") 0

proxyBitWidth :: Proxy h -> proxy x -> Proxy (BitWidth (h x))
proxyBitWidth _ _ = Proxy

type BitRecordOf r h = BitProd r (Field h)
type BitRecord r = BitRecordOf r Identity

instance (Corepresentable p, Comonad (Corep p), Functor f) => Extensible f p (BitProd r) where
  type ExtensibleConstr (BitProd r) h xs x
    = (BitFields r h xs, FromBits r (h x))
  pieceAt i pafb = cotabulate $ \ws -> bupdate i (extract ws) <$> cosieve pafb (blookup i <$> ws)
  {-# INLINE pieceAt #-}
