{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}

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
        fire,
        fire0,
        anytime,
        par,
        parB,
        oneshot,
        now,
        onEnd,
#if defined(MIN_VERSION_arrows)
        -- * Transformer
        -- readerProc
#endif
     )
where

import Prelude hiding (filter)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Foldable as Fd
import qualified Control.Category as Cat
import Control.Monad.Trans
import Control.Monad.State
import Control.Arrow
#if defined(MIN_VERSION_arrows)
import Control.Arrow.Transformer.Reader (ArrowAddReader(..))
#endif
-- import Control.Arrow.Machine.ArrowUtil
import Control.Arrow.Machine.Types

-- $setup
-- >> :set -XArrows



hold ::
    Monad m => b -> ProcessT m (Event b) b
hold old = proc evx ->
  do
    rSwitch (pure old) -< ((), pure <$> evx)

dHold ::
    Monad m => b -> ProcessT m (Event b) b
dHold old = proc evx ->
  do
    drSwitch (pure old) -< ((), pure <$> evx)

-- | Accumulate inputs like fold.
--
-- >> :{
-- let pa = proc evx ->
--       do
--         val <- accum 0 -< (+1) <$ evx
--         returnA -< val <$ evx
--   in
--     run pa (replicate 10 ())
-- :}
-- [1,2,3,4,5,6,7,8,9,10]
--
-- Since 4.0.0, this function become strict for the first argument
-- because lazy one could rarely be used.
--
-- You can make `switch`es to make lazy one.

accum ::
    Monad m => b -> ProcessT m (Event (b->b)) b
accum !x = switch (pure x &&& arr (($x)<$>)) accum'
  where
    accum' y = dSwitch (pure y &&& Cat.id) (const (accum y))

-- | Delayed version of `accum`.
--
-- >> :{
-- let pa = proc evx ->
--       do
--         val <- dAccum 0 -< (+1) <$ evx
--         returnA -< val <$ evx
--   in
--     run pa (replicate 10 ())
-- :}
-- [0,1,2,3,4,5,6,7,8,9]
--
-- Since 4.0.0, this function become strict for the first argument
-- because lazy one could rarely be used.
--
-- You can make `switch`es to make lazy one.

dAccum ::
    Monad m => b -> ProcessT m (Event (b->b)) b
dAccum !x = dSwitch (pure x &&& arr (($x)<$>)) dAccum


-- |Detects edges of input behaviour.
--
-- >> run (hold 0 >>> edge) [1, 1, 2, 2, 2, 3]
-- [0,1,2,3]
--
-- >> run (hold 0 >>> edge) [0, 1, 1, 2, 2, 2, 3]
-- [0,1,2,3]
edge ::
    (Monad m, Eq b) =>
    ProcessT m b (Event b)
edge = evolve $ go Nothing
  where
    go prv =
      do
        cur <- dSwitchAfter $ unsafeExhaust (return . judge prv) >>> (Cat.id &&& Cat.id)
        go $ Just cur
    
    judge prv x = if prv == Just x then Nothing else Just x


-- $sources
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
-- An example is below.
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
-- But a blocking source doesn't stop until its own termination.


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
    (Monad m, Fd.Foldable f) =>
    f a -> ProcessT m (Event i) (Event a)
source l = construct (Fd.mapM_ yd l)
  where
    yd x = await >> yield x

-- | Provides a blocking event stream.
blockingSource ::
    (Monad m, Fd.Foldable f) =>
    f a -> ProcessT m ZeroEvent (Event a)
blockingSource l = arr collapse >>> construct (Fd.mapM_ yield l)

-- | Make a blocking source interleaved.
interleave ::
    Monad m =>
    ProcessT m ZeroEvent (Event a) ->
    ProcessT m (Event i) (Event a)
interleave bs0 = sweep1 (mempty >>> bs0)
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
        (arr collapse >>>) . construct $
          do
            _ <- await
            yield (Just r, bs)
          `catchP`
            yield (Nothing, bs >>> muted)

-- | Make an interleaved source blocking.
blocking ::
    Monad m =>
    ProcessT m (Event ()) (Event a) ->
    ProcessT m ZeroEvent (Event a)
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
    Monad m => ProcessT m (Event b1, Event b2) (Event (Either b1 b2))
tee = proc (e1, e2) -> gather -< [Left <$> e1, Right <$> e2]



-- |Make multiple event channels into one.
-- If simultaneous events are given, lefter one is emitted earlier.
--
-- >> :{
-- let pa = proc x ->
--       do
--         r1 <- filterEvent (\x -> x `mod` 2 == 0) -< x
--         r2 <- filterEvent (\x -> x `mod` 3 == 0) -< x
--         gather -< [r1, r2]
--   in
--     run pa [1..6]
-- :}
-- [2,3,4,6,6]
--
-- It is terminated when the last input finishes.
--
-- >> :{
-- let pa = proc x ->
--       do
--         r1 <- filterEvent (\x -> x `mod` 3 == 0) -< x :: Event Int
--         r2 <- stopped -< x
--         r3 <- returnA -< r2
--         fin <- gather -< [r1, r2, r3]
--         val <- hold 0 -< r1
--         end <- onEnd -< fin
--         returnA -< val <$ end
--   in
--     run pa [1..5]
-- :}
-- [3]

gather ::
    (Monad m, Fd.Foldable f) =>
    ProcessT m (f (Event b)) (Event b)
gather = arr (Fd.foldMap $ fmap singleton) >>> fork
  where
    singleton x = x NonEmpty.:| []


-- |Given an array-valued event and emit it's values as inidvidual events.
--
-- >> run fork [[1,2,3],[],[4,5]]
-- [1,2,3,4,5]
fork ::
    (Monad m, Fd.Foldable f) =>
    ProcessT m (Event (f b)) (Event b)

fork = repeatedly $
    await >>= Fd.mapM_ yield

-- |Executes an action once per an input event is provided.
fire ::
    Monad m =>
    (b -> m c) ->
    ProcessT m (Event b) (Event c)
fire fmy = repeatedlyT $
  do
    x <- await
    y <- lift $ fmy x
    yield y

-- |Executes an action once per an input event is provided.
fire0 ::
    Monad m =>
    m c ->
    ProcessT m (Event ()) (Event c)
fire0 = fire  . const

-- |Executes an action once per an input event is provided.
anytime ::
    ArrowApply a =>
    a b c ->
    ProcessA a (Event b) (Event c)
anytime f = fire (\x -> ArrowMonad (arr (const x) >>> f))

-- |Emit an event of given value as soon as possible.
oneshot ::
    Monad m =>
    c ->
    ProcessT m b (Event c)
oneshot x = arr (const noEvent) >>> go
  where
    go = construct $
        yield x >> forever await

-- |Emit an event as soon as possible.
--
-- @
--  now = oneshot ()
-- @
now ::
    Monad m =>
    ProcessT m b (Event ())
now = oneshot ()

-- |Emit an event at the end of the input stream.
-- >> :{
-- let
--     pa = proc evx ->
--       do
--         x <- hold 0 -< evx
--         ed <- onEnd -< evx
--         returnA -< x <$ ed
--   in
--     run pa [1..10]
-- :}
-- [10]
onEnd ::
    (Monad m, Occasional' b) =>
    ProcessT m b (Event ())
onEnd = arr collapse >>> go
  where
    go = repeatedly $
        await `catchP` (yield () >> stop)


#if defined(MIN_VERSION_arrows)
{-
-- | Run reader of base arrow.
readerProc ::
    (Monad m, Monad m', ArrowAddReader r a a') =>
    ProcessT m b c ->
    ProcessT m' (b, r) c
readerProc pa = arr swap >>> fitW snd (\ar -> arr swap >>> elimReader ar) pa
  where
    swap :: (a, b) -> (b, a)
    swap ~(a, b) = (b, a)
-}
#endif
