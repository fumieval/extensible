{-# LANGUAGE PolyKinds, KindSignatures, TypeOperators, DataKinds, Rank2Types, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Data.Extensible.Tangle where

import Control.Monad.Trans.RWS.Strict
import Control.Monad.Trans.Class
import Data.Extensible.Class
import Data.Extensible.Field
import Data.Extensible.Product
import Data.Extensible.Internal.Rig
import Data.Extensible.Nullable
import Data.Extensible.Wrapper

-- | @'TangleT' h xs m@ is the monad of computations that may depend on the elements in 'xs'.
newtype TangleT h xs m a = TangleT
    { unTangleT :: RWST (Comp (TangleT h xs m) h :* xs) () (Nullable h :* xs) m a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans (TangleT h xs) where
    lift = TangleT . lift

-- | Hitch an element associated to the 'FieldName' through a wrapper.
lasso :: forall k v m h xs. (Monad m, Associate k v xs, Wrapper h)
    => FieldName k -> TangleT h xs m (Repr h (k ':> v))
lasso _ = view _Wrapper <$> hitchAt (association :: Membership xs (k ':> v))
{-# INLINE lasso #-}

-- | Take a value from the tangles. The result is memoized.
hitchAt :: Monad m => Membership xs x -> TangleT h xs m (h x)
hitchAt k = TangleT $ do
    mem <- get
    case getNullable $ hlookup k mem of
        Just a -> return a
        Nothing -> do
            tangles <- ask
            a <- unTangleT $ getComp $ hlookup k tangles
            modify $ over (pieceAt k) $ const $ Nullable $ Just a
            return a

-- | Run tangles and collect the results as a 'Record'.
runTangles :: Monad m
    => Comp (TangleT h xs m) h :* xs -- ^ tangle matrix
    -> Nullable h :* xs -- ^ pre-calculated values
    -> m (h :* xs)
runTangles tangles rec0 = fst <$> evalRWST
    (unTangleT $ htraverseWithIndex (const . hitchAt) rec0) tangles rec0
