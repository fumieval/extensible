{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts #-}
import Data.Extensible
import Control.Lens

mkField "name weight price description featured quantity"

type Stock c = Record '[
    "name" :> String
  , "weight" :> Float
  , "price" :> c
  , "featured" :> Bool
  , "description" :> String
  , "quantity" :> Int]

s0 :: Num c => Stock c
s0 = name @= "DA-192H"
  <: weight @= 260
  <: price @= 120
  <: featured @= True
  <: description @= "High-quality (24bit 192kHz), lightweight portable DAC"
  <: quantity @= 20
  <: Nil

-- Use shrinkAssoc to permute elements
s1 :: Num c => Stock c
s1 = shrinkAssoc
   $ name @= "HHP-150"
  <: featured @= False
  <: description @= "Premium wooden headphone"
  <: price @= 330
  <: quantity @= 55
  <: weight @= 200
  <: Nil

-- If "quantity" is missing,
--    Couldn't match type ‘Missing "quantity"’ with ‘Expecting one’
--
-- If there are duplicate "quantity",
--    Couldn't match type ‘Ambiguous "quantity"’ with ‘Expecting one’

printSummary :: (Associate "name" String s, Associate "description" String s) => Record s -> IO ()
printSummary s = putStrLn $ view name s ++ ": " ++ view description s
