{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
module Data.Extensible.Effect (Action(..)
  , Eff
  , liftEff
  , hoistEff
  , handleWith) where

import Control.Monad.Reader
import Control.Monad.Skeleton
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Extensible.Field
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Extensible.Class
import Data.Extensible.Wrapper (Handler(..))
import Data.Profunctor.Unsafe -- Trustworthy since 7.8

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (foldMap)
#endif

-- | A unit of effects
data Action (xs :: [Assoc k (* -> *)]) a where
  Action :: !(Membership xs kv) -> AssocValue kv a -> Action xs a

-- | The extensible operational monad
type Eff xs = Skeleton (Action xs)

-- | Lift some effect to 'Eff'
liftEff :: forall proxy s t xs a. Associate s t xs => proxy s -> t a -> Eff xs a
liftEff _ x = bone (Action (association :: Membership xs (s ':> t)) x)
{-# INLINE liftEff #-}

hoistEff :: forall proxy s t xs a. Associate s t xs => proxy s -> (forall x. t x -> t x) -> Eff xs a -> Eff xs a
hoistEff _ f = hoistSkeleton $ \(Action i t) -> case compareMembership (association :: Membership xs (s ':> t)) i of
  Right Refl -> Action i (f t)
  _ -> Action i t
{-# INLINABLE hoistEff #-}

handleWith :: RecordOf (Handler m) xs -> Eff xs a -> MonadView m (Eff xs) a
handleWith hs m = case unbone m of
  Action i t :>>= k -> views (pieceAt i) (runHandler .# getField) hs t :>>= k
  Return a -> Return a
{-# INLINABLE handleWith #-}

instance Associate "Reader" (Reader r) xs => MonadReader r (Eff xs) where
  ask = liftEff (Proxy :: Proxy "Reader") ask
  {-# INLINE ask #-}
  local f = hoistEff (Proxy :: Proxy "Reader") (local f)
  {-# INLINE local #-}

instance Associate "State" (State s) xs => MonadState s (Eff xs) where
  get = liftEff (Proxy :: Proxy "State") get
  {-# INLINE get #-}
  put s = liftEff (Proxy :: Proxy "State") (put s)
  {-# INLINE put #-}
  state f = liftEff (Proxy :: Proxy "State") (state f)
  {-# INLINE state #-}

instance (Monoid w, Associate "Writer" (Writer w) xs) => MonadWriter w (Eff xs) where
  writer a = liftEff (Proxy :: Proxy "Writer") (writer a)
  {-# INLINE writer #-}
  tell w = liftEff (Proxy :: Proxy "Writer") (tell w)
  {-# INLINE tell #-}
  listen = go mempty where
    go w m = case unbone m of
      Return a -> return (a, w)
      Action i t :>>= k -> case compareMembership (association :: Membership xs ("Writer" ':> Writer w)) i of
        Right Refl -> bone (Action i t) >>= go (w <> execWriter t) . k
        Left _ -> bone (Action i t) >>= go w . k
  pass m = listen m >>= \((a, f), w) -> writer (a, f w)
  {-# INLINABLE pass #-}
