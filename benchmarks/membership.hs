{-# LANGUAGE ViewPatterns, TypeOperators, GADTs #-}
import Data.Extensible
import Data.Extensible.Internal
import Criterion.Main
import AtoZ
import Unsafe.Coerce

testSumA :: K0 :| AtoZ -> Int
testSumA (UnionAt pos v) = case compareMembership pos (membership :: Membership AtoZ A) of
  Right Refl -> let K0 (A n) = v in n

testSumN :: K0 :| AtoZ -> Int
testSumN (UnionAt pos v) = case compareMembership pos (membership :: Membership AtoZ M) of
  Right Refl -> let K0 (M n) = v in n

testSumT :: K0 :| AtoZ -> Int
testSumT (UnionAt pos v) = case compareMembership pos (membership :: Membership AtoZ T) of
  Right Refl -> let K0 (T n) = v in n

testSumZ :: K0 :| AtoZ -> Int
testSumZ (UnionAt pos v) = case compareMembership pos (membership :: Membership AtoZ Z) of
  Right Refl -> let K0 (Z n) = v in n

data Sum = C0 A|C1 B|C2 C|C3 D|C4 E|C5 F|C6 G|C7 H|C8 I|C9 J|C10 K |C11 L|C12 M
  |C13 N|C14 O|C15 P|C16 Q|C17 R|C18 S|C19 T|C20 U|C21 V|C22 W|C23 X|C24 Y|C25 Z

testC0 :: Sum -> Int
testC0 (C0 (A n)) = n

testC12 :: Sum -> Int
testC12 (C12 (M n)) = n

testC19 :: Sum -> Int
testC19 (C19 (T n)) = n

testC25 :: Sum -> Int
testC25 (C25 (Z n)) = n

main = defaultMain [
  bgroup "sum" [
     bench "A" $ whnf (testSumA . bury) (A 0)
    , bench "M" $ whnf (testSumN . bury) (M 0)
    , bench "T" $ whnf (testSumT . bury) (T 0)
    , bench "Z" $ whnf (testSumZ . bury) (Z 0)
    , bench "A_" $ whnf (testC0 . C0) (A 0)
    , bench "M_" $ whnf (testC12 . C12) (M 0)
    , bench "T_" $ whnf (testC19 . C19) (T 0)
    , bench "Z_" $ whnf (testC25 . C25) (Z 0)
    ]
  , bgroup "product" [
      bench "A" $ whnf (\(pluck -> A x) -> x) blah26
    , bench "G"  $ whnf (\(pluck -> G x) -> x) blah26
    , bench "N"  $ whnf (\(pluck -> N x) -> x) blah26
    , bench "T"  $ whnf (\(pluck -> T x) -> x) blah26
    , bench "Z" $ whnf (\(pluck -> Z x) -> x) blah26
    ]
  , bgroup "tuple" [
      bench "A" $ whnf (\(a,b,c,d,e,f,g,h,i,j,k,l,M res,n,o,p,q,r,s,t,u,v,w,x,y,z) -> res) tuple26
    ]
  ]
