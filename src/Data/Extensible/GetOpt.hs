{-# LANGUAGE TypeFamilies #-}
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
  , getOptRecord) where

import Data.Extensible.Class
import Data.Extensible.Field
import Data.Extensible.Internal.Rig
import Data.Extensible.Product
import Data.Extensible.Wrapper
import Data.List (foldl')
import System.Console.GetOpt

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
