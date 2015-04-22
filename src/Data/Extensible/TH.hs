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
module Data.Extensible.TH (mkField, decFields, decFieldsDeriving, mkEffects) where

import Data.Proxy
import Data.Extensible.Internal.Rig (Optic')
import Data.Extensible.Class (Member, Associate, Extensible, itemAssoc)
import Data.Extensible.Field (FieldOptic)
import Data.Extensible.Plain (accessing)
import Data.Extensible.Effect
import Language.Haskell.TH
import Control.Monad (forM)
import Data.Char (toLower)
import Data.Functor.Identity
import Control.Monad (replicateM)

-- | Generate fields using 'itemAssoc'.
-- @'mkField' "foo bar"@ defines:
--
-- @
-- foo :: FieldOptic "foo"
-- foo = itemAssoc (Proxy :: Proxy "foo")
-- bar :: FieldOptic "bar"
-- bar = itemAssoc (Proxy :: Proxy "bar")
-- @
--
mkField :: String -> DecsQ
mkField str = fmap concat $ forM (words str) $ \s -> do
  let st = litT (strTyLit s)
  let lbl = conE 'Proxy `sigE` (conT ''Proxy `appT` st)
  sequence [sigD (mkName s) $ conT ''FieldOptic `appT` st
    , valD (varP (mkName s)) (normalB $ varE 'itemAssoc `appE` lbl) []
    , return $ PragmaD $ InlineP (mkName s) Inline FunLike AllPhases
    ]

-- | Generate newtype wrappers and lenses from type synonyms.
--
-- @
-- decFields [d|type Foo = Int|]
-- @
--
-- Generates:
--
-- @
-- newtype Foo = Foo Int
-- foo :: (Foo ∈ xs) => Lens' (AllOf xs) Int
-- foo = accessing Foo
-- @
--
decFields :: DecsQ -> DecsQ
decFields = decFieldsDeriving []

-- | 'decFields' with additional deriving clauses
decFieldsDeriving :: [Name] -> DecsQ -> DecsQ
decFieldsDeriving drv' ds = ds >>= fmap concat . mapM mkBody
  where
    mkBody (NewtypeD cx name_ tvs (NormalC nc [(st, ty)]) drv) = do
      let name = let (x:xs) = nameBase name_ in mkName (toLower x : xs)
          xs_ = mkName "xs"
          f_ = mkName "f"
          p_ = mkName "p"
          t_ = mkName "t"
          ext = varT t_ `appT` conT ''Identity `appT` varT xs_
          tvs' = PlainTV xs_ : PlainTV f_ : PlainTV p_ : PlainTV t_ : tvs
      sequence [return $ NewtypeD cx name_ tvs (NormalC nc [(st, ty)]) (drv' ++ drv)

        ,sigD name
#if MIN_VERSION_template_haskell(2,10,0)
          $ forallT tvs' (sequence [conT ''Member `appT` varT xs_ `appT` conT name_
            , conT ''Extensible `appT` varT f_ `appT` varT p_ `appT` varT t_])
#else
          $ forallT tvs' (sequence [classP ''Member [varT xs_, conT name_]
            , classP ''Extensible [varT f_, varT p_, varT t_]])
#endif
          $ conT ''Optic' `appT` varT p_ `appT` varT f_ `appT` ext `appT` return ty

        , valD (varP name) (normalB $ varE 'accessing `appE` conE nc) []
        , return $ PragmaD $ InlineP name Inline FunLike AllPhases
        ]
    mkBody (TySynD name_ tvs ty) = mkBody (NewtypeD [] name_ tvs (NormalC (mkName (nameBase name_)) [(NotStrict, ty)]) [])
    mkBody _ = fail "Unsupported declaration: genField handles newtype declarations or type synonyms"

-- | Generate named effects from a GADT declaration.
mkEffects :: Name -> DecsQ
mkEffects name = reify name >>= \case
  TyConI (DataD _ _ (fmap getTV -> tyvars) cs _)
    | not (null tyvars) -> fmap concat $ forM cs $ \case
      NormalC con st -> mk tyvars [] con st
      ForallC _ eqs (NormalC con st) -> mk tyvars eqs con st
      p -> do
        runIO (print p)
        fail "Unsupported constructor"
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

      let (bts, fts) = foldMap (\(p, t) -> maybe ([VarT t], [t]) (\case
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

      -- Foo a B
      let eff = foldl AppT (ConT name) bts

      -- "Foo"
      let nameT = LitT $ StrTyLit $ nameBase name

#if MIN_VERSION_template_haskell(2,10,0)
      -- Associate "Foo" (Foo a B) xs
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

      let ex = lifter `AppE` foldl AppE (ConE con) (map VarE argNames)

      let fName = let (ch : rest) = nameBase con in mkName $ toLower ch : rest
      return [SigD fName typ
        , FunD fName [Clause (map VarP argNames) (NormalB ex) []]]

    getTV (PlainTV n) = n
    getTV (KindedTV n _) = n
