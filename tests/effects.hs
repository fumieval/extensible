{-# LANGUAGE GADTs, DataKinds, FlexibleContexts, TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
import Data.Extensible

decEffects [d|
  data Example a b x where
    Concrete :: Int -> Example a b ()
    PolyArg :: a -> Example a b ()
    PolyRes :: Example a b b
    PolyArgRes :: a -> Example a b b
    UnboundArg :: x -> Example a b ()
    UnboundRes :: Example a b x
--    ExtArg :: Show s => s -> Example a b ()
--    ExtRes :: Read s => Example a b s
  |]

decEffects [d|
  data Simple x where
    Simple :: Simple ()
  |]

main = return ()
