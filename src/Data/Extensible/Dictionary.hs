{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Extensible.Dictionary where
import Data.Monoid
import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig

dictShow :: forall h xs. WrapForall Show h xs => Match h (Int -> ShowS) :* xs
dictShow = generateFor (Proxy :: Proxy (Instance1 Show h)) $ const $ Match (flip showsPrec)

dictEq :: forall h xs. WrapForall Eq h xs => Wrap2 h Bool :* xs
dictEq = generateFor (Proxy :: Proxy (Instance1 Eq h)) $ const $ Wrap2 (==)

dictOrd :: forall h xs. WrapForall Ord h xs => Wrap2 h Ordering :* xs
dictOrd = generateFor (Proxy :: Proxy (Instance1 Ord h)) $ const $ Wrap2 compare

data WrapMonoid h x = WrapMonoid (h x) (h x -> h x -> h x)

dictMonoid :: forall h xs. WrapForall Monoid h xs => WrapMonoid h :* xs
dictMonoid = generateFor (Proxy :: Proxy (Instance1 Monoid h)) $ const $ WrapMonoid mempty mappend

instance WrapForall Show h xs => Show (h :* xs) where
  showsPrec d = showParen (d > 0)
    . (.showString "Nil")
    . appEndo
    . hfoldMap getConst'
    . hzipWith (\(Match f) h -> Const' $ Endo $ f h 0 . showString " <:* ") dictShow

instance WrapForall Eq h xs => Eq (h :* xs) where
  xs == ys = getAll $ hfoldMap (All . getConst')
    $ hzipWith3 (\(Wrap2 f) x y -> Const' (f x y)) dictEq xs ys

instance (Eq (h :* xs), WrapForall Ord h xs) => Ord (h :* xs) where
  compare xs ys = hfoldMap getConst'
    $ hzipWith3 (\(Wrap2 f) x y -> Const' (f x y)) dictOrd xs ys

instance WrapForall Monoid h xs => Monoid (h :* xs) where
  mempty = hmap (\(WrapMonoid e _) -> e) dictMonoid
  mappend xs ys = hzipWith3 (\(WrapMonoid _ f) x y -> f x y) dictMonoid xs ys

instance WrapForall Show h xs => Show (h :| xs) where
  showsPrec d (UnionAt pos h) = showParen (d > 10) $ showString "embed "
    . runMatch (hlookup pos dictShow) h 11

instance WrapForall Eq h xs => Eq (h :| xs) where
  UnionAt p g == UnionAt q h = case comparePosition p q of
    Left _ -> False
    Right Refl -> unwrap2 (hlookup p dictEq) g h

instance (Eq (h :| xs), WrapForall Ord h xs) => Ord (h :| xs) where
  UnionAt p g `compare` UnionAt q h = case comparePosition p q of
    Left x -> x
    Right Refl -> unwrap2 (hlookup p dictOrd) g h

type WrapForall c h = Forall (Instance1 c h)

-- | Composition for a class and a wrapper
class c (h x) => Instance1 c h x
instance c (h x) => Instance1 c h x
