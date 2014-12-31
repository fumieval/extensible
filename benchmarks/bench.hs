{-# LANGUAGE DataKinds, TypeOperators #-}
import Criterion.Main
import Data.Extensible

data A = A Int deriving Show
data B = B Int deriving Show
data C = C Int deriving Show
data D = D Int deriving Show
data E = E Int deriving Show
data F = F Int deriving Show
data G = G Int deriving Show
data H = H Int deriving Show
data I = I Int deriving Show
data J = J Int deriving Show
data K = K Int deriving Show
data L = L Int deriving Show
data M = M Int deriving Show
data N = N Int deriving Show
data O = O Int deriving Show
data P = P Int deriving Show
data Q = Q Int deriving Show
data R = R Int deriving Show
data S = S Int deriving Show
data T = T Int deriving Show
data U = U Int deriving Show
data V = V Int deriving Show
data W = W Int deriving Show
data X = X Int deriving Show
data Y = Y Int deriving Show
data Z = Z Int deriving Show

blah26 :: K1 :* [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z]
blah26 = K1 (A 0)
    <:* K1 (B 0)
    <:* K1 (C 0)
    <:* K1 (D 0)
    <:* K1 (E 0)
    <:* K1 (F 0)
    <:* K1 (G 0)
    <:* K1 (H 0)
    <:* K1 (I 0)
    <:* K1 (J 0)
    <:* K1 (K 0)
    <:* K1 (L 0)
    <:* K1 (M 0)
    <:* K1 (N 0)
    <:* K1 (O 0)
    <:* K1 (P 0)
    <:* K1 (Q 0)
    <:* K1 (R 0)
    <:* K1 (S 0)
    <:* K1 (T 0)
    <:* K1 (U 0)
    <:* K1 (V 0)
    <:* K1 (W 0)
    <:* K1 (X 0)
    <:* K1 (Y 0)
    <:* K1 (Z 0)
    <:* Nil
