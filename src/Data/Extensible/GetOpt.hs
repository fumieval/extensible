{-# LANGUAGE TypeFamilies, LambdaCase #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.GetOpt
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- A wrapper for 'System.Console.GetOpt'
--------------------------------------------------------------------------------
module Data.Extensible.GetOpt (OptDescr'(..)
  , optNoArg
  , optReqArg
  , getOptRecord
  , withGetOpt) where

import Control.Monad.IO.Class
import Data.Extensible.Class
import Data.Extensible.Field
import Data.Extensible.Internal.Rig
import Data.Extensible.Product
import Data.Extensible.Wrapper
import Data.List (foldl')
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

-- | 'OptDescr' with a default
data OptDescr' a = OptDescr' a (OptDescr (a -> a))

instance Wrapper OptDescr' where
  type Repr OptDescr' a = OptDescr' a
  _Wrapper = id

-- | Option without an argument; the result is the total count of this option.
optNoArg :: [Char] -- ^ short option
    -> [String] -- ^ long option
    -> String -- ^ explanation
    -> OptDescr' Int
optNoArg ss ls expl = OptDescr' 0 $ Option ss ls (NoArg (+1)) expl

-- | Option with an argument
optReqArg :: [Char] -- ^ short option
    -> [String] -- ^ long option
    -> String -- ^ placeholder
    -> String -- ^ explanation
    -> OptDescr' [String]
optReqArg ss ls ph expl = OptDescr' [] $ Option ss ls (ReqArg (:) ph) expl

getOptRecord :: RecordOf OptDescr' xs -- ^ a record of option descriptors
    -> [String] -- ^ arguments
    -> (Record xs, [String], [String], String -> String) -- ^ (result, remaining non-options, errors, usageInfo)
getOptRecord descs args = (foldl' (flip id) def fs, rs, es, flip usageInfo updaters) where
  (fs, rs, es) = getOpt Permute updaters args
  updaters = hfoldrWithIndex
      (\i (Field (OptDescr' _ opt)) -> (:)
          $ fmap (\f -> over (pieceAt i) (Field . fmap f . getField)) opt)
      [] descs
  def = hmap (\(Field (OptDescr' x _)) -> Field (pure x)) descs

-- | When there's an error, print it along with the usage info to stderr
-- and terminate with 'exitFailure'.
withGetOpt :: MonadIO m => RecordOf OptDescr' xs
  -> (Record xs -> [String] -> m a) -> m a
withGetOpt descs k = getOptRecord descs <$> liftIO getArgs >>= \case
  (r, xs, [], _) -> k r xs
  (_, _, errs, usage) -> liftIO $ do
    mapM_ (hPutStrLn stderr) errs
    getProgName >>= die . usage
