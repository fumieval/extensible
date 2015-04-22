{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts #-}
import Data.Extensible

decEffects [d|
  data Example x where
    Foo :: Int -> Example ()
    Bar :: Example String
    Baz :: Bool -> Bool -> Example Int
    |]

mkField "Foo Bar Baz"
