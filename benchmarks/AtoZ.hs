{-# LANGUAGE DataKinds, TypeOperators, GADTs, BangPatterns #-}
module AtoZ where
import Data.Extensible
import Data.Coerce
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

type AtoZ = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z]

blah26 :: K0 :* AtoZ
blah26 = A 0 <% B 1 <% C 2 <% D 3 <% E 4 <% F 5 <% G 6
    <% H 7 <% I 8 <% J 9 <% K 10 <% L 11 <% M 12 <% N 13
    <% O 14 <% P 15 <% Q 16 <% R 17 <% S 18 <% T 19 <% U 20
    <% V 21 <% W 22 <% X 23 <% Y 24 <% Z 25 <% Nil

tuple26 :: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z)
tuple26 = (A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9, K 10, L 11, M 12
  , N 13, O 14, P 15, Q 16, R 17, S 18, T 19, U 20, V 21, W 22, X 23, Y 24, Z 25)

match26 :: Match K0 Int :* AtoZ
match26 = (\(A n) -> n)
  <?% (\(B n) -> n)
  <?% (\(C n) -> n)
  <?% (\(D n) -> n)
  <?% (\(E n) -> n)
  <?% (\(F n) -> n)
  <?% (\(G n) -> n)
  <?% (\(H n) -> n)
  <?% (\(I n) -> n)
  <?% (\(J n) -> n)
  <?% (\(K n) -> n)
  <?% (\(L n) -> n)
  <?% (\(M n) -> n)
  <?% (\(N n) -> n)
  <?% (\(O n) -> n)
  <?% (\(P n) -> n)
  <?% (\(Q n) -> n)
  <?% (\(R n) -> n)
  <?% (\(S n) -> n)
  <?% (\(T n) -> n)
  <?% (\(U n) -> n)
  <?% (\(V n) -> n)
  <?% (\(W n) -> n)
  <?% (\(X n) -> n)
  <?% (\(Y n) -> n)
  <?% (\(Z n) -> n)
  <?% Nil
