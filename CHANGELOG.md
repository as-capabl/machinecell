
4.0.0
----------
### Breaking changes of APIs
* Side-effects are represented by `Monad`s rather than `ArrowApply`ies.
    * Replace the base arrow `ProcessA` with `ProcessT`
    * `ProcessA` is now type alias for compatibility
    * Change the signatures of construction functions
        * `constructT`, `repeatedlyT`
        * `construct`, `repeatedly`
    * Change the signatures of running functions
        * `runT`, `runT_`, `run`, `run_`
        * `stepRun`, `stepYield`
            * Delete `ExecInfo`.
* Change the `Occasional'` type class
    * Add method `burst`
    * Move `noEvent` `end` out of the type class
* Delete `echo`. Use `id` instead.

### Additions
* Add `ZeroEvent`. Change the signatures of blocking sources with it.
* Add `Evolution`
* Add type classes `MonadAwait`, `MonadYield`, `MonadStop`
    * Generalize `await`, `yield`, and `stop` to `Evolution`
* Add `fire`, `fire0`

3.3.2
----------
* Modify again the versions of depending packages.
* Make the default of 'arrow-tr' flag False.

3.3.1
----------
* Modify the versions of depending packages.

3.3.0
----------
* Correct a space leak problem
* Add `splitEvent`, `oneshot`
* Generalize some functions
    * construct, repeatedly
    * filterEvent, filterJust, filterLeft, filterRight

3.2.0
----------
* Add arrow-tr flag
* add `gSwitch`, `dgSwitch`

3.1.0
-----------
* Add `Discrete` utilities
    * eval
    * refer
    * kSwitch
    * dkSwitch
    * Num instance definition
* Add source utilities
    * blockingSource
    * interleave
    * blocking
* Delete `sample`
* Change a switching behavior. With previous implementation, a switching doesn't occur
  when a runnning transducer emits a trigger event using `now` transducer.


3.0.1
-----------
* Fix performance issue of switch, dSwitch, accum, dAccum.

3.0.0
-----------
* ArrowLoop instance now independent of base arrow's
* Make PlanT newtype and add stop handling MonadPlus instance
* API changes
    * Added `filterJust`, `filterLeft`, `filterRight`
    * Deleted Show and Eq instance of Event type
    * Added Isos of ArrowUtil module
    * Delete state monad handling.
    * Delete unsafe primitives `cycleDelay`, `fitEx`, `unsafeSteady`, `loop'`
    * Delete deperecated `passRecent`, `withRecent`
    * Delete ProcessA ArrowReader instance and added `readerProc`


2.1.0
-----------
* Added `dHold`, `dAccum`.
* Deprecated `cycleDelay`.
* Fixed `muted`.
* Slightly changed the ArrowLoop instance declaration.
    * Right tightening rule will be preserved.
    * For IO processes, "Indefinite access to MVar" errors, which used to occur in some
      situations in old versions, will be suppressed.
    * This will not change any existing code unless it loops back
      any Event-type signal.

2.0.1
------------
* Support free-4.12

2.0.0
------------
* Relocate files
    * `catch` and its families are moved to Misc.Exception
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
