{-# LANGUAGE Trustworthy, TemplateHaskell #-}
module Data.Extensible.TH where

import Data.Proxy
import Data.Extensible.Class (Member, Extensible, itemAssoc)
import Data.Extensible.Field (FieldOptic)
import Data.Extensible.Plain (accessing)
import Language.Haskell.TH
import Control.Monad (forM)
import Data.Char (toLower)
import Data.Functor.Identity

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
          q_ = mkName "q"
          t_ = mkName "t"
          ext = varT t_ `appT` conT ''Identity `appT` varT xs_
          tvs' = PlainTV xs_ : PlainTV f_ : PlainTV p_ : PlainTV q_ : PlainTV t_ : tvs
      sequence [return $ NewtypeD cx name_ tvs (NormalC nc [(st, ty)]) (drv' ++ drv)

        ,sigD name
#if MIN_VERSION_template_haskell(2,10,0)
          $ forallT tvs' (sequence [conT ''Member `appT` varT xs_ `appT` conT name_
            , conT ''Extensible `appT` varT f_ `appT` varT p_ `appT` varT q_ `appT` varT t_])
#else
          $ forallT tvs' (sequence [classP ''Member [varT xs_, conT name_]
            , classP ''Extensible [varT f_, varT p_, varT q_, varT t_]])
#endif
          $ arrowT
            `appT` (varT p_ `appT` return ty `appT` (varT f_ `appT` return ty))
            `appT` (varT q_ `appT` ext `appT` (varT f_ `appT` ext))

        , valD (varP name) (normalB $ varE 'accessing `appE` conE nc) []
        , return $ PragmaD $ InlineP name Inline FunLike AllPhases
        ]
    mkBody (TySynD name_ tvs ty) = mkBody (NewtypeD [] name_ tvs (NormalC (mkName (nameBase name_)) [(NotStrict, ty)]) [])
    mkBody _ = fail "Unsupported declaration: genField handles newtype declarations or type synonyms"
