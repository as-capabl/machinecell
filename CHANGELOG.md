2.0.0
------------
* Relocate files
* Performance improve
* Added primitives: `fitEx`, `unsafeSteady`, `unsafeExhaust`
* Added: `condEvent`, `filterEvent`, `muted`
* Added to Misc: `Discrete`, `Pump.asUpdater`, `Pump.Alg`
* Deleted deprecated: `hEv`, `hEv'`, `evMaybe`, `fromEvent`, `split`,
  `join`, `split2`, `join2`, `feedback`, `feedback1`, `isNoEvent`, `isOccasional`, `isEnd`
* Deleted `Foldable` and `Traversable` instance of `Event`.
* Added `Occasional'` by splitting some members from `Occasional`

1.3.1
------------
* (Fix test suite of 1.3.0)

1.3.0
------------
* Support of `ArrowState`.
* Added utilities related to `ArrowLoop` (cycleDelay, Pump)
* Correct EOS behaviour of some utilities.

1.2.0
------------
* Support of `ArrowReader`.
* Added await fail handling.
* Improved performance by church-encoded free monads.
* Arrow stack of newest GHC support for some utilities.

1.1.1
------------
* Eliminated banana brackets to support newest GHC.

1.1.0
------------
* Hide `Event` constructors and some instances (`Applicative`, `Monad`).
* Added `feedback`
* Fixed `accum`

1.0.1
------------
* Fix some bugs of core part.
* Added `onEnd`.
* Added `sample`.

1.0.0
-------------
* First release.
