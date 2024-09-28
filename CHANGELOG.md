0.9.1
------------------------------------------------

* Supported GHCs up to 9.8 ([#38](https://github.com/fumieval/extensible/pull/38) by [@kgtkr](https://github.com/kgtkr))
* Exported `Assoc`, `(>:)`, and `Lookup` from `Data.Extensible.Effect`
* Added `coinclusionAssoc`, `wrenchAssoc`, and `retrenchAssoc` ([#36](https://github.com/fumieval/extensible/pull/36) by [@At-sushi](https://github.com/At-sushi))

0.9
------------------------------------------------

* Removed `FieldName`, the relic of the old ages
    * `(@=)`, `@==`, `@!?` and `lasso` now take `Proxy` instead of `FieldName`. Those who are using `mkField` need to replace the operands with proxies (OverloadedLabels is recommended).
* Supported aeson 1.x
* Introduced `IsLabel` flag which toggles the presence of optics `OverloadedLabels`. By disabling it, this package can now coexist with other users of the `IsLabel` class, such as `generic-lens` and `relational-query`.

0.8.3
------------------------------------------------

* `Comp` is now a pattern synonym for `Compose`
* Added missing `liftTyped` implementations
* Supported aeson-2.0

0.8.1
------------------------------------------------
* Added `DefaultOrdered` and `Incremental` instances to `:&`
* Added an `Incremental` instance to `Field`

0.8
------------------------------------------------

* Removed `Associate`, `AssocKey`, `AssocValue`, `ValueIs`, `KeyIs`, `KeyValue`, `proxyAssocKey`, `proxyAssocValue`, `stringAssokKey`, `xlb`, `:*`, `:|`
* Reverted deprecation of `Data.Extensible.Tangle`

0.7.1
------------------------------------------------
* Removed `vector` and `prettyprinter` orphans
* Deprecated `Data.Extensible.Tangle`; use [tangle](https://hackage.haskell.org/package/tangle)
* `parseJSON` gives more informative failure messages on failure
* Supported `barbies ^>= 2`

0.7
-------------------------------------------------
* Moved `Data.Extensible.Effect` into new `extensible-skeleton` package
* Instances for barbies and cassava are now optional
* Deprecated 訊

0.6.1
-------------------------------------------------
* Added `fromNullable`
* Added `xlb`
* Added a `HasField` instance for `RecordOf`
* Removed `deriveIsRecord`
* Supported GHC 8.8

0.6
-------------------------------------------------
* Added a MonadCont instance for Eff
* `(:*)` and `(:|)` are deprecated in favour of `(:&)` and `(:*)` where their
  type parameters are flipped
* Flipped the type parameters of `BitProd` and `TangleT`
* Added `itemKey`, `hmapWithIndexWith`, `hfoldMapWith`, `hfoldMapWithIndexWith`,
  `hfoldrWithIndexWith`, `hfoldlWithIndexWith`, `hrepeatWith`, `htabulateWith`,
  and `hgenerateWith`

0.5.1
-------------------------------------------------
* Split `Data.Extensible.HList` and `Data.Extensible.Internal` to the
  `membership` package
* `AssocKey`, `AssocValue`, `ValueIs`, `KeyValue` and their related combinators
  are deprecated. Use ones from `membership`
* `IsRecord` now has a generic default implementation
* Deprecated `deriveIsRecord`

0.5
-------------------------------------------------
* GHC older than 8.4 is no longer supported
* Removed `Const'`
* `Data.Extensible.Plain` is no longer exported from `Data.Extensible`
* Added `wrap` and `unwrap` to `Wrapper`
* Added `(=<:)`

0.4.10.1
-------------------------------------------------
* Fixed build on GHC 8.6

0.4.10
-------------------------------------------------
* Added a `MonadResource`, `MonadThrow`, and `MonadCatch` instances for `Eff`
* `Proxy` and `KnownSymbol` are now reexported from `Data.Extensible`

0.4.9
-------------------------------------------------
* Generalised the `MonadIO` instance for `Eff` to `(MonadIO m, Associate "IO" m xs) => MonadIO (Eff xs)`
* Added `And :: (k -> Constraint) -> (k -> Constraint) -> k -> Constraint`
* Added `Semigroup` and `Monoid` instances for `Const'`
* Added `stringAssocKey :: (IsString a, KnownSymbol (AssocKey kv)) => proxy kv -> a`
* Added a `Wrapper` instance for `Either e`
* Added instances of `Pretty` and `Lift`
* Added `hmapWithIndexFor`

0.4.8
-------------------------------------------------
* Changed the `FromJSON` instance for `Record` to call `parseJSON Null` for missing fields
* Added `FromJSON` and `ToJSON` instances for `Nullable (Field h) :* xs`

0.4.7.2
-------------------------------------------------
* Added cassava's `ToNamedRecord`, `ToRecord`, `FromNamedRecord` and `FromRecord` instances
* Added `KeyIs` and `ValueIs`
* Added `FromJSON` and `ToJSON` instances for `(:*)`

0.4.7.1
-------------------------------------------------
* Fixed weird CPP errors on macOS 10.13.2 (#18)
* Added `optFlag`, `optLastArg`, and `optionOptArg`

0.4.7
-------------------------------------------------
* Made various optimisations to improve the compilation time
* Added trivial instances for `FromBits`
* Generalised the API of `Data.Extensible.GetOpt`

0.4.6
-------------------------------------------------
* New module `Data.Extensible.GetOpt`
* Added `fromBitProd`
* Added `Hashable` instances for `:*`, `:|`, `BitProd`, `Membership`, and various wrappers
* Added an `Unbox` instance for `:*`
* Added `hfoldlWithIndex` and `hfoldlWithIndexFor`

0.4.5
-------------------------------------------------
* Added `nothingEff`
* Added `happend`
* Added `Arbitrary` instances for `:*`, `:|`, and wrappers
* Added `Data.Extensible.Bits`

0.4.4
-------------------------------------------------
* Added `contEff` and `runContEff`
* Added `castEff`
* Added `evalStateEff`
* Added `Semigroup` and `Monoid` instances for `Match`, `Comp`, `Prod`
* Added `evalStateDef`, `execStateDef`, and `execWriterDef`
* Added `mkFieldAs`
* Added a `Bounded` instance for `:*`

0.4.3
-------------------------------------------------
* Added `WrappedPointer`
* Added `NFData` and `Generic` instances for `Comp`
* Added a `Semigroup` instance for `h :* xs` and `Membership xs x`
* Added `Prod`
* Added `peelEff0`
* Changed the `IsLabel` instance so that a function is always inferred as an optic
* `Data.Extensible.Class` now exports `compareMembership`
* Renamed `runMembership` to `leadership`

0.4.2
-------------------------------------------------
* Made `newFrom` strict
* `pieceAt` for `(:*)` is now strict
* Added `(<!)`
* Added `peelEff1`, `peelAction0`, `execStateEff`, `execWriterEff`
* Added atomic operations for `Struct`
* Added constrained variants of folds

0.4.1
--------------------------------------------------
* Added `hforce`
* Added an `NFData` instance for `(:*)` and `:|`
* Added a rule to fuse a chain of product updates
* Added a `Monoid` instance for `TangleT`
* Added `(@==)`
* `#foo` can now be overloaded as `FieldOptic "foo"`

0.4
---------------------------------------------------
* Added `Data.Extensible.Struct`
* Changed the representation of `(:*)` to use `SmallArray`
* Removed `(<:*)`. `hhead`, `htail`, `huncons`, `(*++*)`, `htrans`
* New functions: `hfoldrWithIndex`, `hrepeat`, `hrepeatFor`, `haccumMap`,
  `haccum`, `hpartition`, `henumerate`, `hlength`, `hcount`
* Added various derived instances for `Field`
* Added `liftField`, `liftField2`
* Added `Wrapper` instances for `Maybe` and `[]`
* Added `>:` as a synonym for `:>`
* `Data.Extensible.Effect`
  * Refined the API
  * Added `Data.Extensible.Effect.Default`
* Added `Data.Extensible.Tangle`
* Added `record`
* Type inference aids

0.3.7.1
----------------------------------------------------
* `pieceAt` for `(:*)` is now index-preserving
* Removed `sector`, `sectorAt`, `picked`

0.3.7
-----------------------------------------------------
* Support GHC 8.0
* Added a `Monoid` instance for `Field`
* Added `Data.Extensible.Record`
* Added `Enum` and `Bounded` instances for `Proxy :| xs`
* Removed `Data.Extensible.Union`

0.3.6
-----------------------------------------------------
* Added `(@:>)`
* Added `(!-!!)`, `nihility`, `squash`

0.3.5
-----------------------------------------------------
* Added `Data.Extensible.Effect`
* Added `decEffects`

0.3.4
-----------------------------------------------------
* Added `Data.Extensible.Wrapper`
* Added `itemAt`, `item`, `itemAssoc`
* Safe Haskell
* Generalized `Field`

0.3.3
-----------------------------------------------------
* Renamed `sectorAt`, `sector`, `sectorAssoc` to `pieceAt`, `piece`, `pieceAssoc`, respectively
* `picked` is now subsumed by `piece`
    * `mkField` yields more generalized optics
* Renamed `UnionAt` to `EmbedAt`
* Removed `clause`; Use `piece . _Match`
* Removed `record`; Use `piece . _K0`
* Added `htraverseWithIndex`
* Renamed `ord` to `mkMembership`
* Fixed the `Show` instance of `:|`
* Added `Variant`

0.3.2
-----------------------------------------------------
* Added `Associate` class and combinators around it
* `Data.Extensible.Record` now lets values be independent from keys
    * `mkField` requires 1 argument
* Added `Data.Extensible.Union`, partially taking `elevator`'s functionality
* Removed old `Data.Extensible.Union` and `Data.Extensible.League`
* Removed `(<?!)`

0.3.1
-----------------------------------------------------
* Removed `Reifiable`
* Now `library` yields desired dictionaries
* Added `remember`
* Added `strike` and `strikeAt`

0.3
-----------------------------------------------------
* Renamed `generate` to `htabulate`
* Renamed `generateA` to `hgenerate`
* Renamed `generateFor` to `htabulateFor`
* Renamed `generateForA` to `hgenerateFor`
* Renamed `htabulate` to `hmapWithIndex`
* Added `(<@=>)`
* Added `Comp`
* Fixed badly-specialized `htraverse`
* Added `hsequence`, `hdistribute`, `hcollect`
* Added `hindex`

0.2.10
-----------------------------------------------------
* Optimized `sector` (~2x)

0.2.9
-----------------------------------------------------
* Renamed `(<?~)` to `(<?!$)`
* Renamed `(<$?~)` to `(<?!~)`
* Refactored `Data.Extensible.Dictionary`
* Supported serialization/deserialization of products using `binary`

0.2.8
-----------------------------------------------------
* Improved performance considerably

0.2.7
-----------------------------------------------------
* Added `accessing`
* Added `decFields` and `decFieldsDeriving`
* Renamed `Position` to `Membership`

0.2.6
-----------------------------------------------------
* Right-associated `(++)`
* Added `htrans`
* Added `recordType`
* Made Eq, Ord, Show instances for Position more reasonable

0.2.5
-----------------------------------------------------
* Added `(<:)`
* Re-exported `Data.Extensible.Record`, `Data.Extensible.Union`, `Data.Extensible.League`
* Brushed instances up
* Added `subset`
* Added `Data.Extensible.Internal.HList` and combinators

0.2.4
------------------------------------------------------
* Corrected the definition of `Half`
* Added `coinclusion`, `wrench`, `retrench` along with `Nullable`
* Added `htabulate`

0.2.3
-------------------------------------------------------
* Corrected the behavior of `Generate` and `Forall`
* Made type errors more readable
* Added `(*++*)`
* Fixed the accidental miscall of `getUnion`

0.2.2
--------------------------------------------------------
* Added `recordAt`
* Added `ord`
* Re-added `K1`
* Toggled INLINE pragmas

0.2.1
--------------------------------------------------------
* Added `hhead` and `htail`
* Changed the definition of `Union` to use coyoneda style

0.2
--------------------------------------------------------
* Split modules up
* Flipped `Position`
* Added several combinators
