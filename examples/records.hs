{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, KindSignatures, TypeFamilies, FlexibleContexts #-}
import Data.Extensible.Record
import Data.Extensible
import Control.Lens
import Data.Proxy
import Data.Functor.Identity

mkField "name" [t|String|]
mkField "weight" [t|Float|]
mkField "price" [t|Int|]
mkField "description" [t|String|]
mkField "featured" [t|Bool|]
mkField "quantity" [t|Int|]

type Stock = Record '["name", "weight", "price", "featured", "description", "quantity"]

s0 :: Stock
s0 = Field "DA-192H"
  <:* Field 260
  <:* Field 120
  <:* Field True
  <:* Field "High-quality (24bit 192kHz), lightweight portable DAC"
  <:* Field 20
  <:* Nil

-- Use shrink to permute elements
s1 :: Stock
s1 = shrink $ name @= "HHP-150"
  <:* featured @= False
  <:* description @= "Premium wooden headphone"
  <:* weight @= 150
  <:* price @= 330
  <:* quantity @= 10
  <:* Nil

printSummary :: ("name" ∈ s, "description" ∈ s) => Record s -> IO ()
printSummary s = putStrLn $ view name s ++ ": " ++ view description s
