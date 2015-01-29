{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts #-}

import Data.Extensible
import Control.Lens
decFieldsDeriving [''Show, ''Eq, ''Ord] [d|
  type Name = String
  type Weight = Float
  type Price = Int
  type Description = String
  type Featured = Bool
  type Quantity = Int
  |]

type Stock = AllOf '[Name, Weight, Price, Featured, Description, Quantity]

s0 :: Stock
s0 = Name "DA-192H"
  <% Weight 260
  <% Price 120
  <% Featured True
  <% Description "High-quality (24bit 192kHz), lightweight portable DAC"
  <% Quantity 20
  <% Nil

-- Use shrink to permute elements
s1 :: Stock
s1 = shrink
   $ Name "HHP-150"
  <% Featured False
  <% Description "Premium wooden headphone"
  <% Weight 150
  <% Price 330
  <% Quantity 55
  <% Nil

-- If Quantity is missing,
--    Couldn't match type ‘Missing Quantity’ with ‘Expecting one’
--
-- If there are duplicate Quantity,
--    Couldn't match type ‘Ambiguous Quantity’ with ‘Expecting one’

printSummary :: (Name ∈ s, Description ∈ s) => AllOf s -> IO ()
printSummary s = putStrLn $ view name s ++ ": " ++ view description s
