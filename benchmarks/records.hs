{-# LANGUAGE TemplateHaskell, TypeOperators, DataKinds, FlexibleContexts #-}
import Criterion.Main
import Control.Lens
import Data.Extensible
import Data.Monoid

mkField "foo bar baz qux foobar foobaz fooqux barfoo barbaz barqux"

type Fields = ["foo" >: Sum Int
  , "bar" >: String
  , "baz" >: First Int
  , "qux" >: String
  , "foobar" >: (Sum Int, String)
  , "foobaz" >: (Sum Int, First Int)
  , "fooqux" >: (Sum Int, String)
  , "barfoo" >: (String, Sum Int)
  , "barbaz" >: (String, First Int)
  , "barqux" >: (String, String)]

recA :: Record Fields
recA = foo @= Sum 1 <: bar @= "barA" <: baz @= mempty <: qux @= "qux"
    <: foobar @= (Sum 1, "foobar")
    <: foobaz @= (Sum 5, mempty)
    <: fooqux @= (Sum 6, mempty)
    <: barfoo @= mempty
    <: barbaz @= mempty
    <: barqux @= mempty
    <: nil
{-# NOINLINE recA #-}

recB :: Record Fields
recB = foo @= Sum 2 <: bar @= "barB" <: baz @= pure 42 <: qux @= "qux"
  <: foobar @= (Sum 1, "foobar")
  <: foobaz @= (Sum 5, mempty)
  <: fooqux @= (Sum 7, mempty)
  <: barfoo @= mempty
  <: barbaz @= mempty
  <: barqux @= mempty
  <: nil
{-# NOINLINE recB #-}

data HsRec = HsRec { _hsFoo :: Sum Int, _hsBar :: String, _hsBaz :: First Int
  , _hsQux :: String
  , _hsFooBar :: (Sum Int, String)
  , _hsFooBaz :: (Sum Int, First Int)
  , _hsFooQux :: (Sum Int, String)
  , _hsBarFoo :: (String, Sum Int)
  , _hsBarBaz :: (String, First Int)
  , _hsBarQux :: (String, String)
  }
makeLenses ''HsRec

hsRec = HsRec { _hsFoo = Sum 1, _hsBar = "hsBar"
  , _hsBaz = mempty, _hsQux = "hsQux"
  , _hsFooBar = (Sum 1, "foobar")
  , _hsFooBaz = (Sum 5, mempty)
  , _hsFooQux = (Sum 6, mempty)
  , _hsBarFoo = mempty
  , _hsBarBaz = mempty
  , _hsBarQux = mempty
  }

main = defaultMain
  [ bgroup "basic"
    [ bench "view" $ whnf (view foo) recA
    , bench "hsview" $ whnf (view hsFoo) hsRec
    , bench "set" $ whnf (set foo 3) recB
    , bench "hsset" $ whnf (set hsFoo 3) hsRec
    ]
  , bgroup "instances"
    [ bench "mappend" $ whnf (uncurry mappend) (recA, recB)
    , bench "==" $ whnf (uncurry (==)) $! (recA, recB)
    , bench "compare" $ whnf (uncurry compare) (recA, recB)
    , bench "show" $ nf show recA
    ]
  ]
