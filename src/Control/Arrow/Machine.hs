{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

{-|
Module: Control.Arrow.Machine
Description: Contains the main documentation and module imports.
-}
module
    Control.Arrow.Machine
      (
        -- * Quick introduction
        -- $introduction

        -- * Note
        -- $note

        -- * Modules
        -- | "Control.Arrow.Machine" is good to import qualified, because no operators are exported.
        --
        -- Alternatively, you can import libraries below individually,
        -- with only "Control.Arrow.Machine.Utils" qualified or identifier specified.
        --
        -- Control.Arrow.Machine.Misc.* are not included by default.
        -- They are all designed to import qualified.
        module Control.Arrow.Machine.ArrowUtil,
        module Control.Arrow.Machine.Types,
        module Control.Arrow.Machine.Utils
       )
where

import Control.Arrow.Machine.ArrowUtil
import Control.Arrow.Machine.Types
import Control.Arrow.Machine.Utils

-- $introduction
-- As other iteratee or pipe libraries, machinecell abstracts general iteration processes.
--
-- Here is an example that is a simple iteration over a list.
--
-- >>> run (evMap (+1)) [1, 2, 3]
-- [2, 3, 4]
--
-- In above statement, "`evMap` (+1)" has a type "ProcessA (-\>) (Event Int) (Event Int)",
-- which denotes "A stream transducer that takes a series of Int as input,
-- gives a series of Int as output, run on base arrow (-\>)."
--
-- `ProcessA` is the transducer type of machinecell library.
--
-- = Side effects
--
-- In general, `Arrow` types other than (-\>) may have side effects.
-- For example any monadic side effects can be performed by wrapping the monad with `Kleisli`.
--
-- ProcessA can run the effects as following.
--
-- >>> runKleisli (run_ $ anytime (Kleisli print)) [1, 2, 3]
-- 1
-- 2
-- 3
--
--  Where `anytime` makes a transducer that executes side effects for each input.
-- `run_` is almost same as `run` but discards transducer's output.
--
-- That is useful in the case rather side effects are main concern.
--
-- = ProcessA as pipes
--
-- "ProcessA a (Event b) (Event c)" transducers are actually one-directional composable pipes.
--
-- They can be constructed from the `Plan` monad.
-- In `Plan` monad context, `await` and `yield` can be used to get and emit values.
-- And actions of base monads can be `lift`ed to the context.
--
-- @
-- source :: ProcessA (Kleisli IO) (Event ()) (Event String)
-- source = repeatedlyT kleisli0 $
--   do
--     _ \<- await
--     x \<- lift getLine
--     yield x
--
-- pipe :: ArrowApply a =\> ProcessA a (Event String) (Event String)
-- pipe = construct $
--   do
--     s1 \<- await
--     s2 \<- await
--     yield (s1 ++ s2)
--
-- sink :: ProcessA (Kleisli IO) (Event String) (Event Void)
-- sink = repeatedlyT kleisli0
--   do
--     x \<- await
--     lift $ putStrLn x
-- @
--
-- Then, resulting processes are composed as `Category` using `(\>\>\>)` operator.
--
-- > runKleisli (run_ $ source >>> pipe >>> sink) (repeat ())
--
-- This reads two lines from stdin, puts a concatenated line to stdout and finishes.
--
-- Unlike other pipe libraries, even the source calls `await`.
-- The source awaits dummy input, namely "(repeat ())", and discard input values.
--
-- Even the input is an infinite list, this program stops when the "pipe" transducer stops.
--
-- == More details on finalizing
--
-- Finalizing behavior of transducers obey the following scenario.
--
-- 1. Signals of type `Event` can carry /end signs/.
-- 2. Most transducers stop when they get an end sign.
--    (Some exceptions can be made by `onEnd` or `catchP`)
-- 3. If `run` function detects an end sign as an output of a running transducer,
--    it stops feeding input values and alternatively feeds end signs.
-- 4. Continue iteration until no more events can be occurred.
--
-- So "await \`catchP\` some_cleanup" can handle any stop of both upstream and downstream.
--
-- On the other hand, a plan never gets end sign without calling await.
-- So it is better that even a source calls await.
--
-- A source that calls await periodically is an "interleaved source".
-- Interleaved sources have a number of advantages.
-- They can be controled their output timings by their upstream, or can be stopped any time.
--
-- There is another kind of source that doesn't call await, namely "blocking source".
--
-- see "sources" section of "Control.Arrow.Machine.Utils" documentation.
--
-- = Arrow composition
--
-- One of the most attractive feature of machinecell is the /arrow composition/.
--
-- In addition to `Category`, ProcessA has `Arrow` instance declaration,
-- which allows parallel compositions.
--
-- If a type has an `Arrow` instance, it can be wrote by ghc extended proc-do notation as following.
--
-- @
-- f :: ProcessA (Kleisli IO) (Event Int) (Event ())
-- f = proc x -\>
--   do
--     -- Process odd integers.
--     odds \<- filter $ arr odd -\< x
--     anytime $ Kleisli (putStrLn . ("Odd: " ++)) -\< show \<$\> odds
--
--     -- Process even integers.
--     evens \<- filter $ arr even -\< x
--     anytime $ Kleisli (putStrLn . ("Even: " ++)) -\< show \<$\> evens
-- @
--
-- >>> P.runKleisli (run f) [1..10]
-- Odd: 1
-- Even: 2
-- Odd: 3
-- Even: 4
-- ...
--
-- The result implies that two statements that inputs x and their downstreams are
-- executed in parallel.
--
-- = Behaviours
--
-- The transducers we have already seen are all have input and output type wrapped by `Event`.
-- We have not taken care of them so far because all of them are cancelled each other.
--
-- But several built-in transducers provide non-event values like below.
--
-- @
-- hold :: ArrowApply a =\> b -\> ProcessA a (Event b) b
-- accum :: ArrowApply a =\> b -\> ProcessA a (Event (b-\>b)) b
-- @
--
-- `hold` keeps the last input until a new value is provided.
--
-- `accum` updates its outputting by applying every input function.
--
-- According to a knowledge from arrowized FRP(functional reactive programming),
-- values that appear naked in arrow notations are /behaviour/,
-- that means /coutinuous/ time-varying values,
-- whereas /event/ values are /discrete/.
--
-- Note that all values that can be input, output, or taken effects must be discrete.
--
-- To use continuous values anyhow interacting the real world,
-- they must be encoded to discrete values.
--
-- That's done by functor calculations between any existing events.
--
-- An example is below.
--
-- @
-- f :: ArrowApply a =\> ProcessA a (Event Int) (Event Int)
-- f = proc x -\>
--    do
--      y \<- accum 0 -\< (+) \<$\> x
--      returnA -\< y \<$ x
-- @
--
-- >>> run f [1, 2, 3]
-- [1, 3, 6]
--
-- `(\<$)` operator discards the value of rhs and only uses that's container structure
-- e.g. 1 \<$ Just "a" =\> Just 1, 1 \<$ Nothing =\> Nothing,
-- 1 \<$ [True, False, undefined] =\> [1, 1, 1].
--
-- In this case, the value of y are outputed according to the timing of x.
--



-- $note
-- = Purity of `ProcessA (-\>)`
-- Since the 1st type parameter of `ProcessA` represents base monad(ArrowApply),
-- "ProcessA (-\>)" is expected to be pure.
--
-- In other words, the following arrow results the same result for arbitrary f.
--
-- @
-- proc x -\>
--   do
--     _ \<- `fit` arr f -\< x
--     g -\< x
-- @
--
-- Which is desugared to "fit arr f &&& g \>\>\> arr snd". At least if `Event` constructor is exported,
-- someone can make a counter example.
-- When f is "arr (replicate k) \>\>\> fork" for some integer k and g is "arr (const $ Event ())",
-- g yields ()s for k times. That is because, the result value of arrow "f &&& g" is
-- nothing but "(Event x, Event ())" and its number of yields is k because "Event x" must
-- be yielded k times.
--
-- This is the reason why the `Event` constructor is hidden.
-- Using exported primitives, it works almost correctly.
-- Event number is conserved by inserting an appropriate number of `NoEvent`s.
-- But there is still a loophole.
--
-- Under the current implementation, the arrow below behaves like "arr (const $ Event x)".
--
-- @
-- proc x -\> hold noEvent -\< ev \<$ ev
-- @
--
-- I have an idea to correct this, such that the above arrow always be `NoEvent`.
-- But in the result `Event` is no longer a functor in the meaning of haskell type class.
--
-- For now, if you never make value of nested event type like "ev \<$ ev",
-- the problem will be avoided.
--
-- = Looping
--
-- Although `ProcessA` is an instance of `ArrowLoop`,
-- there is a large limitation.
--
-- The limitation is, Events mustn't be looped back to upstream.
--
-- In example below, result is [0, 0, 0, 0], not [1, 2, 3, 4].
--
-- @
-- f = proc x -\>
--   do
--     rec
--         b \<- hold 0 -\< y
--         y \<- fork -\< (\xx -\> [xx, xx+1, xx+2, xx+3]) \<$\> x
--     returnA -\< b \<$ y
--
-- dHold i = proc x -\> drSwitch (pure i) -\< ((), pure \<$\> x)
-- @
--
-- >>> run f [1]
-- [0, 0, 0, 0]
--
-- In general, `Event` values refered at upstream in rec statements are
-- almost always `NoEvent`s.
--
-- A better way to send events to upstream is, to encode them to behaviours using `dHold`,
-- `dAccum` and so on, then send to upstream in rec statement.
--

