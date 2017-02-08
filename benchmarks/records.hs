{-# LANGUAGE TemplateHaskell, TypeOperators, DataKinds, FlexibleContexts #-}
import Criterion.Main
import Control.Lens
import Data.Extensible
import Data.Monoid

mkField "foo bar baz qux foobar foobaz fooqux"

type Fields = ["foo" >: Sum Int
  , "bar" >: String
  , "baz" >: First Int
  , "qux" >: String
  , "foobar" >: (Sum Int, String)
  , "foobaz" >: (Sum Int, First Int)
  , "fooqux" >: (Sum Int, String)]

recA :: Record Fields
recA = foo @= Sum 1 <: bar @= "barA" <: baz @= mempty <: qux @= "qux"
    <: foobar @= (Sum 1, "foobar")
    <: foobaz @= (Sum 5, mempty)
    <: fooqux @= (Sum 6, mempty)
    <: nil
{-# NOINLINE recA #-}

recB :: Record Fields
recB = foo @= Sum 2 <: bar @= "barB" <: baz @= pure 42 <: qux @= "qux"
  <: foobar @= (Sum 1, "foobar")
  <: foobaz @= (Sum 5, mempty)
  <: fooqux @= (Sum 7, mempty)
  <: nil
{-# NOINLINE recB #-}

data HsRec = HsRec { hsFoo :: !(Sum Int), hsBar :: !String, hsBaz :: !(First Int)
  , hsQux :: !String
  , hsFooBar :: !(Sum Int, String)
  , hsFooBaz :: !(Sum Int, First Int)
  , hsFooQux :: !(Sum Int, String)
  }

hsRec = HsRec { hsFoo = Sum 1, hsBar = "hsBar"
  , hsBaz = mempty, hsQux = "hsQux"
  , hsFooBar = (Sum 1, "foobar")
  , hsFooBaz = (Sum 5, mempty)
  , hsFooQux = (Sum 6, mempty)
  }

main = defaultMain
  [ bgroup "basic"
    [ bench "view" $ whnf (view foo) recA
    , bench "set" $ whnf (set foo 3) recB
    , bench "hsset" $ whnf (\r -> r { hsFoo = 3}) hsRec
    ]
  , bgroup "instances"
    [ bench "mappend" $ whnf (uncurry mappend) (recA, recB)
    , bench "==" $ whnf (uncurry (==)) $! (recA, recB)
    , bench "compare" $ whnf (uncurry compare) (recA, recB)
    , bench "show" $ nf show recA
    ]
  ]
