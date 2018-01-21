{-# LANGUAGE LambdaCase, TemplateHaskell, TypeFamilies #-}
------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Record
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Bidirectional conversion from/to records
------------------------------------------------------------------------
module Data.Extensible.Record (IsRecord(..), toRecord, fromRecord, record, deriveIsRecord) where

import Language.Haskell.TH
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Extensible.HList
import Data.Extensible.Product
import Data.Extensible.Field
import Data.Functor.Identity
import Data.Profunctor
import GHC.TypeLits

-- | The class of types that can be converted to/from a 'Record'.
class IsRecord a where
  type RecFields a :: [Assoc Symbol *]
  recordFromList :: HList (Field Identity) (RecFields a) -> a
  recordToList :: a -> HList (Field Identity) (RecFields a)

instance IsRecord () where
  type RecFields () = '[]
  recordFromList _ = ()
  recordToList _ = HNil

-- | Convert a value into a 'Record'.
toRecord :: IsRecord a => a -> Record (RecFields a)
toRecord = fromHList . recordToList
{-# INLINE toRecord #-}

-- | Convert a 'Record' to a value.
fromRecord :: IsRecord a => Record (RecFields a) -> a
fromRecord = recordFromList . toHList
{-# INLINE fromRecord #-}

-- | @record :: IsRecord a => Iso' a (Record (RecFields a)) @
record :: (IsRecord a, Functor f, Profunctor p)
  => Optic' p f a (Record (RecFields a))
record = dimap toRecord (fmap fromRecord)
{-# INLINE record #-}

tvName :: TyVarBndr -> Name
tvName (PlainTV n) = n
tvName (KindedTV n _) = n

-- | Create an 'IsRecord' instance for a normal record declaration.
deriveIsRecord :: Name -> DecsQ
deriveIsRecord name = reify name >>= \case
#if MIN_VERSION_template_haskell(2,11,0)
  TyConI (DataD _ _ vars _ [RecC conName vst] _) -> do
#else
  TyConI (DataD _ _ vars [RecC conName vst] _) -> do
#endif
    let names = [x | (x, _, _) <- vst]
    newNames <- traverse (newName . nameBase) names
    let tvmap = [(tvName tv, VarT (mkName $ "p" ++ show i)) | (i, tv) <- zip [0 :: Int ..] vars]
    let ty = foldl AppT (ConT name) $ map snd tvmap
    let refineTV (VarT t) | Just t' <- lookup t tvmap = t'
        refineTV (AppT a b) = refineTV a `AppT` refineTV b
        refineTV t = t
    return
#if MIN_VERSION_template_haskell(2,11,0)
      [InstanceD Nothing [] (ConT ''IsRecord `AppT` ty)
#else
      [InstanceD [] (ConT ''IsRecord `AppT` ty)
#endif
        [ TySynInstD ''RecFields $ TySynEqn [ty] $ foldr
            (\(v, _, t) r -> PromotedConsT `AppT` (PromotedT '(:>) `AppT` LitT (StrTyLit $ nameBase v) `AppT` refineTV t) `AppT` r)
            PromotedNilT
            vst
        , FunD 'recordFromList [Clause
            [shape2Pat $ fmap (\x -> ConP 'Field [ConP 'Identity [VarP x]]) newNames]
            (NormalB $ RecConE conName [(n, VarE n') | (n, n') <- zip names newNames])
            []
            ]
        , FunD 'recordToList [Clause
            [ConP conName (map VarP newNames)]
            (NormalB $ shape2Exp [AppE (ConE 'Field)
                $ AppE (ConE 'Identity)
                $ VarE n
              | n <- newNames])
            []
            ]
        ]
      ]
  info -> fail $ "deriveIsRecord: Unsupported " ++ show info

shape2Pat :: [Pat] -> Pat
shape2Pat [] = ConP 'HNil []
shape2Pat (x : xs) = ConP 'HCons [x, shape2Pat xs]

shape2Exp :: [Exp] -> Exp
shape2Exp [] = ConE 'HNil
shape2Exp (x : xs) = ConE 'HCons `AppE` x `AppE` shape2Exp xs
