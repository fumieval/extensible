{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, OverloadedLabels #-}

import Control.Lens
import Data.Extensible
import Data.Extensible.Bits
import Data.Word
import Test.QuickCheck

type Fields = ["a" >: Bool, "b" >: Word8, "c" >: Bool]

type Rec = BitRecord Word64 Fields

main = do
  quickCheck $ \x r -> set #a x (BitProd r :: Rec) ^. #a == x
  quickCheck $ \b -> let r = BitProd b :: Rec
    in set #a (r ^. #a) r == r
  quickCheck $ \b x y -> let r = BitProd b :: Rec
    in set #a x (set #a y r) == set #a x r

  quickCheck $ \x r -> set #b x (BitProd r :: Rec) ^. #b == x
  quickCheck $ \b -> let r = BitProd b :: Rec
    in set #b (r ^. #b) r == r
  quickCheck $ \b x y -> let r = BitProd b :: Rec
    in set #b x (set #b y r) == set #b x r

  quickCheck $ \x r -> set #c x (BitProd r :: Rec) ^. #c == x
  quickCheck $ \b -> let r = BitProd b :: Rec
    in set #c (r ^. #c) r == r
  quickCheck $ \b x y -> let r = BitProd b :: Rec
    in set #c x (set #c y r) == set #c x r
