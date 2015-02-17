{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, PolyKinds, ViewPatterns, TypeFamilies, TypeOperators, GADTs, Rank2Types, ScopedTypeVariables, DataKinds #-}
import Data.Extensible
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Control.Applicative
import Criterion.Main
import AtoZ
import Data.HList hiding (K(..))
import Unsafe.Coerce

data Sum = C0 A|C1 B|C2 C|C3 D|C4 E|C5 F|C6 G|C7 H|C8 I|C9 J|C10 K |C11 L|C12 M
  |C13 N|C14 O|C15 P|C16 Q|C17 R|C18 S|C19 T|C20 U|C21 V|C22 W|C23 X|C24 Y|C25 Z

testNaive :: Sum -> Int
testNaive (C0 (A n)) = n
testNaive (C1 (B n)) = n
testNaive (C2 (C n)) = n
testNaive (C3 (D n)) = n
testNaive (C4 (E n)) = n
testNaive (C5 (F n)) = n
testNaive (C6 (G n)) = n
testNaive (C7 (H n)) = n
testNaive (C8 (I n)) = n
testNaive (C9 (J n)) = n
testNaive (C10 (K n)) = n
testNaive (C11 (L n)) = n
testNaive (C12 (M n)) = n
testNaive (C13 (N n)) = n
testNaive (C14 (O n)) = n
testNaive (C15 (P n)) = n
testNaive (C16 (Q n)) = n
testNaive (C17 (R n)) = n
testNaive (C18 (S n)) = n
testNaive (C19 (T n)) = n
testNaive (C20 (U n)) = n
testNaive (C21 (V n)) = n
testNaive (C22 (W n)) = n
testNaive (C23 (X n)) = n
testNaive (C24 (Y n)) = n
testNaive (C25 (Z n)) = n

testExt :: K0 :| AtoZ -> Int
testExt = match match26

main = defaultMain [
   bgroup "product" [
     bench "Data"  $ whnf (\(getZ -> Z x) -> x) data26
    , bench "A" $ whnf (\(pluck -> A x) -> x) extensible26
    , bench "B"  $ whnf (\(pluck -> B x) -> x) extensible26
    , bench "C"  $ whnf (\(pluck -> C x) -> x) extensible26
    , bench "D"  $ whnf (\(pluck -> D x) -> x) extensible26
    , bench "E"  $ whnf (\(pluck -> E x) -> x) extensible26
    , bench "F"  $ whnf (\(pluck -> F x) -> x) extensible26
    , bench "G"  $ whnf (\(pluck -> G x) -> x) extensible26
    , bench "H"  $ whnf (\(pluck -> H x) -> x) extensible26
    , bench "I"  $ whnf (\(pluck -> I x) -> x) extensible26
    , bench "J"  $ whnf (\(pluck -> J x) -> x) extensible26
    , bench "K"  $ whnf (\(pluck -> K x) -> x) extensible26
    , bench "L"  $ whnf (\(pluck -> L x) -> x) extensible26
    , bench "M"  $ whnf (\(pluck -> M x) -> x) extensible26
    , bench "N"  $ whnf (\(pluck -> N x) -> x) extensible26
    , bench "O"  $ whnf (\(pluck -> O x) -> x) extensible26
    , bench "P"  $ whnf (\(pluck -> P x) -> x) extensible26
    , bench "Q"  $ whnf (\(pluck -> Q x) -> x) extensible26
    , bench "R"  $ whnf (\(pluck -> R x) -> x) extensible26
    , bench "S"  $ whnf (\(pluck -> S x) -> x) extensible26
    , bench "T"  $ whnf (\(pluck -> T x) -> x) extensible26
    , bench "U"  $ whnf (\(pluck -> U x) -> x) extensible26
    , bench "V"  $ whnf (\(pluck -> V x) -> x) extensible26
    , bench "W"  $ whnf (\(pluck -> W x) -> x) extensible26
    , bench "X"  $ whnf (\(pluck -> X x) -> x) extensible26
    , bench "Y"  $ whnf (\(pluck -> Y x) -> x) extensible26
    , bench "Z"  $ whnf (\(pluck -> Z x) -> x) extensible26
    , bench "A"  $ whnf (\(hOccursFst -> A x) -> x) hlist26
    , bench "B"  $ whnf (\(hOccursFst -> B x) -> x) hlist26
    , bench "C"  $ whnf (\(hOccursFst -> C x) -> x) hlist26
    , bench "D"  $ whnf (\(hOccursFst -> D x) -> x) hlist26
    , bench "E"  $ whnf (\(hOccursFst -> E x) -> x) hlist26
    , bench "F"  $ whnf (\(hOccursFst -> F x) -> x) hlist26
    , bench "G"  $ whnf (\(hOccursFst -> G x) -> x) hlist26
    , bench "H"  $ whnf (\(hOccursFst -> H x) -> x) hlist26
    , bench "I"  $ whnf (\(hOccursFst -> I x) -> x) hlist26
    , bench "J"  $ whnf (\(hOccursFst -> J x) -> x) hlist26
    , bench "K"  $ whnf (\(hOccursFst -> K x) -> x) hlist26
    , bench "L"  $ whnf (\(hOccursFst -> L x) -> x) hlist26
    , bench "M"  $ whnf (\(hOccursFst -> M x) -> x) hlist26
    , bench "N"  $ whnf (\(hOccursFst -> N x) -> x) hlist26
    , bench "O"  $ whnf (\(hOccursFst -> O x) -> x) hlist26
    , bench "P"  $ whnf (\(hOccursFst -> P x) -> x) hlist26
    , bench "Q"  $ whnf (\(hOccursFst -> Q x) -> x) hlist26
    , bench "R"  $ whnf (\(hOccursFst -> R x) -> x) hlist26
    , bench "S"  $ whnf (\(hOccursFst -> S x) -> x) hlist26
    , bench "T"  $ whnf (\(hOccursFst -> T x) -> x) hlist26
    , bench "U"  $ whnf (\(hOccursFst -> U x) -> x) hlist26
    , bench "V"  $ whnf (\(hOccursFst -> V x) -> x) hlist26
    , bench "W"  $ whnf (\(hOccursFst -> W x) -> x) hlist26
    , bench "X"  $ whnf (\(hOccursFst -> X x) -> x) hlist26
    , bench "Y"  $ whnf (\(hOccursFst -> Y x) -> x) hlist26
    , bench "Z"  $ whnf (\(hOccursFst -> Z x) -> x) hlist26
    ]
  , bgroup "sum" [
     bench "A" $ whnf testExt (bury (A 0))
    , bench "M" $ whnf testExt (bury (M 0))
    , bench "T" $ whnf testExt (bury (T 0))
    , bench "Z" $ whnf testExt (bury (Z 0))
    , bench "A_" $ whnf testNaive  (C0 (A 0))
    , bench "M_" $ whnf testNaive (C12 (M 0))
    , bench "T_" $ whnf testNaive (C19 (T 0))
    , bench "Z_" $ whnf testNaive (C25 (Z 0))
    ]

  ]
