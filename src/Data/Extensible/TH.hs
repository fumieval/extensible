{-# LANGUAGE Trustworthy, TemplateHaskell, LambdaCase, ViewPatterns #-}
------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.TH
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
------------------------------------------------------------------------
module Data.Extensible.TH (mkField
  , mkFieldAs) where

import Data.Extensible.Class (itemAssoc)
import Data.Extensible.Field
import Language.Haskell.TH
import Data.Char
import Control.Monad
import Type.Membership

-- | Generate fields using 'itemAssoc'.
-- @'mkField' "foo Bar"@ defines:
--
-- @
-- foo :: FieldOptic "foo"
-- foo = itemAssoc (Proxy :: Proxy "foo")
-- _Bar :: FieldOptic "Bar"
-- _Bar = itemAssoc (Proxy :: Proxy "Bar")
-- @
--
mkField :: String -> DecsQ
mkField str = fmap concat $ forM (words str) $ \s@(x:xs) ->
  let name = mkName $ if isLower x then x : xs else '_' : x : xs
  in mkFieldAs name s

-- | @'mkFieldAs' (mkName "foo") "bar"@ defines a field for "bar" as @foo@.
mkFieldAs :: Name -> String -> DecsQ
mkFieldAs name s = do
  let st = litT (strTyLit s)
  let lbl = conE 'Proxy `sigE` (conT ''Proxy `appT` st)
  sequence [sigD name $ conT ''FieldOptic `appT` st
    , valD (varP name) (normalB $ varE 'itemAssoc `appE` lbl) []
    , return $ PragmaD $ InlineP name Inline FunLike AllPhases
    ]
