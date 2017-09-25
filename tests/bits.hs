{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Data.Bits
import Data.Extensible
import Data.Extensible.Bits
import Data.Int
import Data.Proxy
import Data.Word
import GHC.TypeLits
import Language.Haskell.TH (mkName)
import Test.QuickCheck

type Fields = ["a" >: Bool, "b" >: Word8, "c" >: Bool]

mkFieldAs (mkName "_a") "a"
mkFieldAs (mkName "_b") "b"
mkFieldAs (mkName "_c") "c"


prop_lensA1 x r = set _a x (BitProd r :: Rec) ^. _a == x
prop_lensA2 b = let r = BitProd b :: Rec
  in set _a (r ^. _a) r == r
prop_lensA3 b x y = let r = BitProd b :: Rec
  in set _a x (set _a y r) == set _a x r

prop_lensB1 x r = set _b x (BitProd r :: Rec) ^. _b == x
prop_lensB2 b = let r = BitProd b :: Rec
  in set _b (r ^. _b) r == r
prop_lensB3 b x y = let r = BitProd b :: Rec
  in set _b x (set _b y r) == set _b x r

prop_lensC1 x r = set _c x (BitProd r :: Rec) ^. _c == x
prop_lensC2 b = let r = BitProd b :: Rec
  in set _c (r ^. _c) r == r
prop_lensC3 b x y = let r = BitProd b :: Rec
  in set _c x (set _c y r) == set _c x r

type Rec = BitRecord Word64 Fields

focus :: forall a. (Eq a, Show a) => FromBits Word64 a => Word64 -> a -> Property
focus x a = let w = fromIntegral $ natVal (Proxy :: Proxy (BitWidth a))
  in fromBits (x `shiftL` w .|. toBits a) === a

clean :: forall a. Eq a => FromBits Word64 a => a -> Property
clean a = let w = fromIntegral $ natVal (Proxy :: Proxy (BitWidth a))
  in toBits a `shiftR` w === (zeroBits :: Word64)

prop_focus_Word8 :: Word64 -> Word8 -> Property
prop_focus_Word8 = focus

prop_clean_Word8 :: Word8 -> Property
prop_clean_Word8 = clean

prop_focus_Word16 :: Word64 -> Word16 -> Property
prop_focus_Word16 = focus

prop_clean_Word16 :: Word16 -> Property
prop_clean_Word16 = clean

prop_focus_Word32 :: Word64 -> Word32 -> Property
prop_focus_Word32 = focus

prop_clean_Word32 :: Word32 -> Property
prop_clean_Word32 = clean

prop_focus_Int8 :: Word64 -> Int8 -> Property
prop_focus_Int8 = focus

prop_clean_Int8 :: Int8 -> Property
prop_clean_Int8 = clean

prop_focus_Int16 :: Word64 -> Int16 -> Property
prop_focus_Int16 = focus

prop_clean_Int16 :: Int16 -> Property
prop_clean_Int16 = clean

prop_focus_Int32 :: Word64 -> Int32 -> Property
prop_focus_Int32 = focus

prop_clean_Int32 :: Int32 -> Property
prop_clean_Int32 = clean

prop_focus_Bool :: Word64 -> Bool -> Property
prop_focus_Bool = focus

prop_clean_Bool :: Bool -> Property
prop_clean_Bool = clean

return []
main = $quickCheckAll
