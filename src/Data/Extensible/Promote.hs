{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Extensible.Promote where

import Data.Extensible.Product
import Data.Extensible.Wrapper
import Data.Proxy

class Promote h a where
  type Promotion h a :: *
  promote :: h a -> h (Promotion h a)

newtype Promoted h x = Promoted { unPromoted :: h (Promotion h x) }

instance Wrapper h => Wrapper (Promoted h) where
  type Repr (Promoted h) x = Repr h (Promotion h x)
  wrap = Promoted . wrap
  unwrap = unwrap . unPromoted

promoteRecord :: forall h xs. Forall (Promote h) xs => h :* xs -> Promoted h :* xs
promoteRecord = hmapWithIndexFor (Proxy :: Proxy (Promote h))
  $ const $ Promoted . promote
