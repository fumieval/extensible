{-# LANGUAGE UndecidableInstances, ScopedTypeVariables #-}
module Data.Extensible.Internal.Dictionary where
import Data.Monoid
import Data.Extensible.Product
import Data.Extensible.Internal.Rig
import Data.Proxy

data WrapMonoid h x = WrapMonoid (h x) (h x -> h x -> h x)

dictShow :: forall h xs. Forall (ClassComp Show h) xs => Match h (Int -> ShowS) :* xs
dictShow = generateFor (Proxy :: Proxy (ClassComp Show h)) $ const $ Match (flip showsPrec)

dictEq :: forall h xs. Forall (ClassComp Eq h) xs => Wrap2 h Bool :* xs
dictEq = generateFor (Proxy :: Proxy (ClassComp Eq h)) $ const $ Wrap2 (==)

dictOrd :: forall h xs. Forall (ClassComp Ord h) xs => Wrap2 h Ordering :* xs
dictOrd = generateFor (Proxy :: Proxy (ClassComp Ord h)) $ const $ Wrap2 compare

dictMonoid :: forall h xs. Forall (ClassComp Monoid h) xs => WrapMonoid h :* xs
dictMonoid = generateFor (Proxy :: Proxy (ClassComp Monoid h)) $ const $ WrapMonoid mempty mappend

instance Forall (ClassComp Eq h) xs => Eq (h :* xs) where
  xs == ys = getAll $ hfoldMap (All . getConst')
    $ hzipWith3 (\(Wrap2 f) x y -> Const' (f x y)) dictEq xs ys

instance (Forall (ClassComp Eq h) xs, Forall (ClassComp Ord h) xs) => Ord (h :* xs) where
  compare xs ys = hfoldMap getConst'
    $ hzipWith3 (\(Wrap2 f) x y -> Const' (f x y)) dictOrd xs ys
