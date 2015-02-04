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
