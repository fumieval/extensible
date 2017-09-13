{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module Data.Extensible.Effect.Pipe (
    Yield(..)
    , Await(..)
    , (>->)
    , yield
    , await
    , leftover
    -- * Named
    , pipeEff
    , yieldAs
    , awaitAs
    , leftoverAs) where

import Control.Monad.Skeleton
import Data.Extensible.Internal
import Data.Extensible.Effect

data Yield e x where
  Yield :: e -> Yield e ()

data Await e x where
  Await :: Await e (Maybe e)
  Leftover :: e -> Await e ()

uncons :: Eff ((k ':> Yield e) ': xs) a -> Eff xs (Either a (e, Eff ((k ':> Yield e) ': xs) a))
uncons m = case debone m of
  Return a -> return $ Left a
  Instruction i t :>>= k -> leadership i
    (\Refl -> case t of
      Yield e -> return $ Right (e, k ()))
    (\j -> boned $ Instruction j t :>>= uncons . k)

terminate :: forall k e xs b. Eff ((k ':> Await e) ': xs) b -> Eff xs b
terminate = peelEff1 (const . return)
  (\t k lo -> case t of
    Await -> case lo of
      [] -> k Nothing []
      x : xs -> k (Just x) xs
    Leftover e -> k () (e : lo)) `flip` ([] :: [e])

pipeEff :: forall yield await e xs a b
  . Eff ((yield ':> Yield e) ': xs) a
  -> Eff ((await ':> Await e) ': xs) b -> Eff xs b
pipeEff = go [] where
  go :: [e] -> Eff ((yield ':> Yield e) ': xs) a
    -> Eff ((await ':> Await e) ': xs) b -> Eff xs b
  go lo p c = case debone c of
    Return b -> return b
    Instruction i t :>>= k -> leadership i
      (\Refl -> case t of
        Await -> case lo of
          x : xs -> go xs p $ k (Just x)
          [] -> uncons p >>= \case
            Left _ -> terminate $ k Nothing
            Right (x, p') -> go [] p' $ k (Just x)
        Leftover e -> go (e : lo) p $ k ())
      (\j -> boned $ Instruction j t :>>= go lo p . k)
{-# INLINE pipeEff #-}

(>->) :: Eff (("Yield" ':> Yield e) ': xs) a
  -> Eff (("Await" ':> Await e) ': xs) b
  -> Eff xs b
(>->) = pipeEff
{-# INLINE (>->) #-}

awaitAs :: Associate k (Await e) xs => Proxy k -> Eff xs (Maybe e)
awaitAs p = liftEff p Await
{-# INLINE awaitAs #-}

await :: (Associate "Await" (Await e) xs, Associate "EOF" MaybeEff xs) => Eff xs e
await = awaitAs (Proxy :: Proxy "Await")
  >>= maybe (throwEff (Proxy :: Proxy "EOF") ()) return
{-# INLINE await #-}

leftoverAs :: Associate k (Await e) xs => Proxy k -> e -> Eff xs ()
leftoverAs p = liftEff p . Leftover
{-# INLINE leftoverAs #-}

leftover :: Associate "Await" (Await e) xs => e -> Eff xs ()
leftover = leftoverAs (Proxy :: Proxy "Await")
{-# INLINE leftover #-}

yieldAs :: Associate k (Yield e) xs => Proxy k -> e -> Eff xs ()
yieldAs p = liftEff p . Yield
{-# INLINE yieldAs #-}

yield :: Associate "Yield" (Yield e) xs => e -> Eff xs ()
yield = liftEff (Proxy :: Proxy "Yield") . Yield
{-# INLINE yield #-}
