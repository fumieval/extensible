{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts #-}
import Data.Extensible

decEffects [d|
  data Example x where -- the name doesn't matter
    Foo :: Int -> Example ()
    Bar :: Example String
    Baz :: Bool -> Bool -> Example Int
    |]

mkField "Foo Bar Baz"

test :: (Associate "Foo" (Action '[Int] ()) xs
  , Associate "Bar" (Action '[] String) xs
  , Associate "Baz" (Action '[Bool, Bool] Int) xs) => Eff xs Int
test = do
  foo 42
  s <- bar
  t <- bar
  baz (s == "bar") (s == t)
