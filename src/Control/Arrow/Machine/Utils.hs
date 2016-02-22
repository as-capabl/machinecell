{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

module
    Control.Arrow.Machine.Utils
      (
        -- * AFRP-like utilities
        hold,
        dHold,
        accum,
        dAccum,
        edge,

        -- * Switches
        -- | Switches inspired by Yampa library.
        -- Signature is almost same, but collection requirement is  not only 'Functor',
        -- but 'Tv.Traversable'. This is because of side effects.
        switch,
        dSwitch,
        rSwitch,
        drSwitch,
        kSwitch,
        dkSwitch,
        pSwitch,
        pSwitchB,
        rpSwitch,
        rpSwitchB,

        -- * Sources
        -- $sources
        source,
        blockingSource,
        interleave,
        blocking,

        -- * Other utility arrows
        tee,
        gather,
        fork,
        filter,
        echo,
        anytime,
        par,
        parB,
        now,
        onEnd,

        -- * Transformer
        readerProc
     )
where

import Prelude hiding (filter)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Foldable as Fd
import qualified Control.Category as Cat
import Control.Monad.Trans
import Control.Monad.State
import Control.Arrow
import Control.Applicative
import Control.Arrow.Transformer.Reader (ArrowAddReader(..))

import Control.Arrow.Machine.ArrowUtil
import Control.Arrow.Machine.Types





hold ::
    ArrowApply a => b -> ProcessA a (Event b) b
hold old = proc evx ->
  do
    rSwitch (pure old) -< ((), pure <$> evx)

dHold ::
    ArrowApply a => b -> ProcessA a (Event b) b
dHold old = proc evx ->
  do
    drSwitch (pure old) -< ((), pure <$> evx)

accum ::
    ArrowApply a => b -> ProcessA a (Event (b->b)) b
accum x = switch (pure x &&& arr (($x)<$>)) accum'
  where
    accum' y = dSwitch (pure y &&& Cat.id) (const (accum y))

dAccum ::
    ArrowApply a => b -> ProcessA a (Event (b->b)) b
dAccum x = dSwitch (pure x &&& arr (($x)<$>)) dAccum


edge ::
    (ArrowApply a, Eq b) =>
    ProcessA a b (Event b)
edge = proc x ->
  do
    rec
        ev <- unsafeExhaust (arr judge) -< (prv, x)
        prv <- dHold Nothing -< Just x <$ ev
    returnA -< ev
  where
    judge (prv, x) = if prv == Just x then Nothing else Just x


-- &sources
-- In addition to the main event stream privided by `run`,
-- there are two other ways to provide additional input streams,
-- "interleaved" sources and "blocking" sources.
--
-- Interleaved sources are actually Event -> Event transformers
-- that don't see the values of the input events.
-- They discard input values and emit their values according to input event timing.
--
-- Blocking sources emit their events independent from upstream.
-- Until they exhaust their values, they block upstream transducers.
--
-- Here is a demonstration of two kind of sources.
--
-- @
-- a = proc x ->
--   do
--     y1 <- source [1, 2, 3] -< x
--     y2 <- source [4, 5, 6] -< x
--
--     gather -< [y1, y2]
-- -- run a (repeat ()) => [1, 4, 2, 5, 3, 6]
--
-- b = proc _ ->
--   do
--     y1 <- blockingSource [1, 2, 3] -< ()
--     y2 <- blockingSource [4, 5, 6] -< ()
--
--     gather -< [y1, y2]
-- -- run b [] => [4, 5, 6, 1, 2, 3]
-- @
--
-- In above code, you'll see that output values of `source`
-- (an interleaved source) are actually interelaved,
-- while `blockingSource` blocks another upstream source.
--
-- And they can both implemented using `PlanT`.
-- The only one deference is `await` call to listen upstream event timing.
--
-- An example is follows.
--
-- @
-- interleavedStdin = constructT kleisli0 (forever pl)
--   where
--     pl =
--       do
--         _ <- await
--         eof <- isEOF
--         if isEOF then stop else return()
--         getLine >>= yield
--
-- blockingStdin = pure noEvent >>> constructT kleisli0 (forever pl)
--   where
--     pl =
--       do
--         -- No await here
--         eof <- isEOF
--         if isEOF then stop else return()
--         getLine >>= yield
-- @
--
-- They are different in the end behavior.
-- When upstream stops, an interleaved source stops because await call fails.
-- But a blocking source don't stop until its own termination.


-- | Provides a source event stream.
-- A dummy input event stream is needed.
--
-- @
--   run af [...]
-- @
--
-- is equivalent to
--
-- @
--   run (source [...] >>> af) (repeat ())
-- @
source ::
    (ArrowApply a, Fd.Foldable f) =>
    f c -> ProcessA a (Event b) (Event c)
source l = construct $ Fd.mapM_ yd l
  where
    yd x = await >> yield x

-- | Provides a blocking event stream.
blockingSource ::
    (ArrowApply a, Fd.Foldable f) =>
    f c -> ProcessA a () (Event c)
blockingSource l = pure noEvent >>> construct (Fd.mapM_ yield l)

-- | Make a blocking source interleaved.
interleave ::
    ArrowApply a =>
    ProcessA a () (Event c) ->
    ProcessA a (Event b) (Event c)
interleave bs0 = sweep1 (pure () >>> bs0)
  where
    waiting bs r =
        dSwitch
            (handler bs r)
            sweep1
    sweep1 bs =
        kSwitch
            bs
            (arr snd)
            waiting
    handler bs r = proc ev ->
      do
        ev' <- splitter bs r -< ev
        returnA -< (filterJust (fst <$> ev'), snd <$> ev')
    splitter bs r =
        construct $
          do
            _ <- await
            yield (Just r, bs)
          `catchP`
            yield (Nothing, bs >>> muted)

-- | Make an interleaved source blocking.
blocking ::
    ArrowApply a =>
    ProcessA a (Event ()) (Event c) ->
    ProcessA a () (Event c)
blocking is = dSwitch (blockingSource (repeat ()) >>> is >>> (Cat.id &&& onEnd)) (const stopped)


--
-- other utility arrow

-- |Make two event streams into one.
-- Actually `gather` is more general and convenient;
--
-- @... \<- tee -\< (e1, e2)@
--
-- is equivalent to
--
-- @... \<- gather -\< [Left \<$\> e1, Right \<$\> e2]@
--
tee ::
    ArrowApply a => ProcessA a (Event b1, Event b2) (Event (Either b1 b2))
tee = proc (e1, e2) -> gather -< [Left <$> e1, Right <$> e2]



-- |Make multiple event channels into one.
-- If simultaneous events are given, lefter one is emitted earlier.
gather ::
    (ArrowApply a, Fd.Foldable f) =>
    ProcessA a (f (Event b)) (Event b)
gather = arr (Fd.foldMap $ fmap singleton) >>> fork
  where
    singleton x = x NonEmpty.:| []


-- |Given an array-valued event and emit it's values as inidvidual events.
fork ::
    (ArrowApply a, Fd.Foldable f) =>
    ProcessA a (Event (f b)) (Event b)

fork = repeatedly $
    await >>= Fd.mapM_ yield

-- |Executes an action once per an input event is provided.
anytime ::
    ArrowApply a =>
    a b c ->
    ProcessA a (Event b) (Event c)

anytime action = repeatedlyT (ary0 unArrowMonad) $
  do
    x <- await
    ret <- lift $ arrowMonad action x
    yield ret


filter ::
    ArrowApply a =>
    a b Bool ->
    ProcessA a (Event b) (Event b)
filter cond = repeatedlyT (ary0 unArrowMonad) $
  do
    x <- await
    b <- lift $ arrowMonad cond x
    if b then yield x else return ()


echo ::
    ArrowApply a =>
    ProcessA a (Event b) (Event b)

echo = filter (arr (const True))

now ::
    ArrowApply a =>
    ProcessA a b (Event ())
now = arr (const noEvent) >>> go
  where
    go = construct $
        yield () >> forever await

onEnd ::
    (ArrowApply a, Occasional' b) =>
    ProcessA a b (Event ())
onEnd = arr collapse >>> go
  where
    go = repeatedly $
        await `catchP` (yield () >> stop)

-- | Run reader of base arrow.
readerProc ::
    (ArrowApply a, ArrowApply a', ArrowAddReader r a a') =>
    ProcessA a b c ->
    ProcessA a' (b, r) c
readerProc pa = arr swap >>> fitW snd (\ar -> arr swap >>> elimReader ar) pa
  where
    swap :: (a, b) -> (b, a)
    swap ~(a, b) = (b, a)

