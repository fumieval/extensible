{-# LANGUAGE Trustworthy, TemplateHaskell, LambdaCase, ViewPatterns #-}
------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.TH
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
------------------------------------------------------------------------
module Data.Extensible.TH (mkField, decEffects) where

import Data.Proxy
import Data.Extensible.Internal
import Data.Extensible.Class (itemAssoc)
import Data.Extensible.Effect
import Data.Extensible.Field
import Data.List (nub)
import Language.Haskell.TH
import Data.Char
import Control.Monad

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (foldMap)
#endif

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
mkField str = fmap concat $ forM (words str) $ \s@(x:xs) -> do
  let st = litT (strTyLit s)
  let name = mkName $ if isLower x then x : xs else '_' : x : xs
  let lbl = conE 'Proxy `sigE` (conT ''Proxy `appT` st)
  sequence [sigD name $ conT ''FieldOptic `appT` st
    , valD (varP name) (normalB $ varE 'itemAssoc `appE` lbl) []
    , return $ PragmaD $ InlineP name Inline FunLike AllPhases
    ]

allVars :: Type -> [Name]
allVars (AppT s t) = allVars s ++ allVars t
allVars (VarT n) = [n]
allVars _ = []

-- | Generate named effects from a GADT declaration.
decEffects :: DecsQ -> DecsQ
decEffects decs = decs >>= \ds -> fmap concat $ forM ds $ \case
#if MIN_VERSION_template_haskell(2,11,0)
  DataD _ dataName (fmap getTV -> tyvars) _ cs _
#else
  DataD _ dataName (fmap getTV -> tyvars) cs _
#endif
    | not (null tyvars) -> do
      (cxts, dcs) <- fmap unzip $ forM cs $ \case
        NormalC con st -> mk tyvars [] con st
        ForallC _ eqs (NormalC con st) -> mk tyvars eqs con st
        p -> do
          runIO (print p)
          fail "Unsupported constructor"
      let vars = map PlainTV $ nub $ concatMap (allVars . snd) cxts
      return $ TySynD dataName vars (foldr
        (\(k, v) xs -> PromotedConsT `AppT` (PromotedT '(:>) `AppT` k `AppT` v) `AppT` xs) PromotedNilT cxts)
          : concat dcs
  _ -> fail "mkEffects accepts GADT declaration"
  where
    mk tyvars eqs con (fmap snd -> argTypes) = do
#if MIN_VERSION_template_haskell(2,10,0)
      let dic_ = [(v, t) | AppT (AppT EqualityT (VarT v)) t <- eqs]
#else
      let dic_ = [(v, t) | EqualP (VarT v) t <- eqs]
#endif
      let dic = dic_ ++ [(t, VarT v) | (v, VarT t) <- dic_]

      let tvs = map mkName $ concatMap (flip replicateM ['a'..'z']) [1..]

      let params' = do
            (t, v) <- zip tyvars tvs
            case lookup t dic of
              Just (VarT p) -> return (t, p)
              _ -> return (t, v)

      let (_, fts) = foldMap (\(p, t) -> maybe ([VarT t], [t]) (\case
              VarT _ -> ([VarT t], [t])
              x -> ([x], [])) (lookup p dic)) (init params')

      let argTypes' = map (\case
            VarT n -> maybe (VarT n) VarT $ lookup n params'
            x -> x) argTypes

      let (extra, result) = case lookup (last tyvars) dic of
            Just (VarT v) -> (id, case lookup v params' of
              Just p -> VarT p
              Nothing -> VarT v)
            Just t -> (id, t)
            Nothing -> ((PlainTV (mkName "x"):), VarT $ mkName "x")

      -- Eff xs R
      let rt = ConT ''Eff `AppT` VarT (mkName "xs") `AppT` result

      -- a -> B -> C -> Eff xs R
      let fun = foldr (\x y -> ArrowT `AppT` x `AppT` y) rt argTypes'

      -- Action [a, B, C] R
      let eff = ConT ''Action
            `AppT` foldr (\x y -> PromotedConsT `AppT` x `AppT` y) PromotedNilT argTypes'
            `AppT` result

      -- "Foo"
      let nameT = LitT $ StrTyLit $ nameBase con

      -- Associate "Foo" (Foo a B C) xs
#if MIN_VERSION_template_haskell(2,10,0)
      let cx = ConT ''Associate
            `AppT` nameT
            `AppT` eff
            `AppT` VarT (mkName "xs")
#else
      let cx = ClassP ''Associate [nameT, eff, VarT (mkName "xs")]
#endif

      let typ = ForallT (PlainTV (mkName "xs") : extra (map PlainTV fts)) [cx] fun

      -- liftEff (Proxy :: Proxy "Foo")
      let lifter = VarE 'liftEff `AppE` (ConE 'Proxy `SigE` AppT (ConT ''Proxy) nameT)

      let argNames = map (mkName . ("a" ++) . show) [0..length argTypes-1]

      let ex = lifter
            `AppE` foldr (\x y -> ConE 'AArgument `AppE` x `AppE` y)
                         (ConE 'AResult)
                         (map VarE argNames)

      let fName = let (ch : rest) = nameBase con in mkName $ toLower ch : rest
      return ((nameT, eff), [SigD fName typ
        , FunD fName [Clause (map VarP argNames) (NormalB ex) []]])

    getTV (PlainTV n) = n
    getTV (KindedTV n _) = n
