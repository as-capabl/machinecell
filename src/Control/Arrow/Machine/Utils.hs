{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module
    Control.Arrow.Machine.Utils
      (
        -- * AFRP-like utilities
        hold,
        accum,
        edge,
        passRecent,
        withRecent,

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

        -- * State arrow
        peekState,
        encloseState,

        -- * Other utility arrows
        tee,
        gather,
        sample,
        source,
        fork,
        filter,
        echo,
        anytime,
        par,
        parB,
        now,
        onEnd,
        cycleDelay
       )
where

import Prelude hiding (filter)

import Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Foldable as Fd
import qualified Control.Category as Cat
import Control.Monad.Trans
import Control.Monad.State
import Control.Arrow
import Control.Arrow.Operations (ArrowState(..))
import Control.Arrow.Transformer.State (ArrowAddState(..), StateArrow())
import Control.Applicative

import Control.Arrow.Machine.ArrowUtil
import Control.Arrow.Machine.Types
import Control.Arrow.Machine.Exception





hold :: 
    ArrowApply a => b -> ProcessA a (Event b) b
{-
hold old = ProcessA $ proc (ph, evx) ->
  do
    let new = fromEvent old evx
    returnA -< (ph `mappend` Suspend, new, hold new)
-}
hold old = proc evx -> 
  do
    rSwitch (arr $ const old) -< ((), arr . const <$> evx)

accum ::
    ArrowApply a => b -> ProcessA a (Event (b->b)) b
accum i = encloseState (go >>> peekState) i
  where
    go = repeatedlyT (ary0 $ statefully unArrowMonad) $ await >>= modify
    
  

edge :: 
    (ArrowApply a, Eq b) =>
    ProcessA a b (Event b)

edge = encloseState (unsafeExhaust impl) Nothing
  where
    impl ::
        (ArrowApply a, Eq b) =>
        StateArrow (Maybe b) a b (Maybe b)
    impl = proc x ->
      do
        mprv <- fetch -< ()
        store -< Just x
        returnA -<
            case mprv
              of
                Just prv -> if prv == x then Nothing else Just x
                Nothing -> Just x

{-# DEPRECATED passRecent, withRecent "Use `hold` instead" #-}
infixr 9 `passRecent`

passRecent :: 
    (ArrowApply a, Occasional o) =>
    ProcessA a (AS e) (Event b) ->
    ProcessA a (e, AS b) o ->
    ProcessA a (AS e) o

passRecent af ag = proc ase ->
  do
    evx <- af -< ase
    mvx <- hold Nothing -< Just <$> evx
    case mvx of
      Just x -> ag -< (fromAS ase, toAS x)
      _ -> returnA -< noEvent

withRecent :: 
    (ArrowApply a, Occasional o) =>
    ProcessA a (e, AS b) o ->
    ProcessA a (e, AS (Event b)) o
withRecent af = proc (e, asevx) ->
  do
    mvx <- hold Nothing -< Just <$> fromAS asevx
    case mvx of
      Just x -> af -< (e, toAS x)
      _ -> returnA -< noEvent






--
-- State arrow
--
peekState ::
    (ArrowApply a, ArrowState s a) =>
    ProcessA a e s
peekState = unsafeSteady fetch

-- Should be exported?
exposeState ::
    (ArrowApply a, ArrowApply a', ArrowAddState s a a') =>
    ProcessA a b c ->
    ProcessA a' (b, s) (c, s)
exposeState = fitEx es
  where
    es f = proc (p, (x, s)) ->
      do
        ((q, y), s') <- elimState f -< ((p, x), s)
        returnA -< (q, (y, s'))

encloseState ::
    (ArrowApply a, ArrowApply a', ArrowAddState s a a') =>
    ProcessA a b c ->
    s ->
    ProcessA a' b c
encloseState pa s = loop' s (exposeState pa)

--
-- other utility arrow

-- |Make two event streams into one.
-- Actually `gather` is more general and convenient;
-- 
-- @... <- tee -< (e1, e2)@
-- 
-- is equivalent to
-- 
-- @... <- gather -< [Left <$> e1, Right <$> e2]@
-- 
tee ::
    ArrowApply a => ProcessA a (Event b1, Event b2) (Event (Either b1 b2))
tee = proc (e1, e2) -> gather -< [Left <$> e1, Right <$> e2]



sample ::
    ArrowApply a =>
    ProcessA a (Event b1, Event b2) [b1]
{-
sample = join >>> construct (go id) >>> hold []
  where
    go l = 
      do
        (evx, evy) <- await `catch` return (NoEvent, End)
        let l2 = evMaybe l (\x -> l . (x:)) evx
        if isEnd evy
          then
          do
            yield $ l2 []
            stop
          else
            return ()
        evMaybe (go l2) (\_ -> yield (l2 []) >> go id) evy
-}
sample = undefined

-- |Make multiple event channels into one.
-- If simultaneous events are given, lefter one is emitted earlier.
gather ::
    (ArrowApply a, Fd.Foldable f) =>
    ProcessA a (f (Event b)) (Event b)
gather = arr (Fd.foldMap $ fmap singleton) >>> fork
  where
    singleton x = x NonEmpty.:| []

-- | Provides a source event stream.
-- A dummy input event stream is needed. 
-- @
--   run af [...]
-- @
-- is equivalent to
-- @
--   run (source [...] >>> af) (repeat ())
-- @
source ::
    (ArrowApply a, Fd.Foldable f) =>
    f c -> ProcessA a (Event b) (Event c)
source l = construct $ Fd.mapM_ yd l
  where
    yd x = await >> yield x

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
    (ArrowApply a, Occasional b) =>
    ProcessA a b (Event ())
onEnd = arr collapse >>> go
  where
    go = repeatedly $
        await `catch` (yield () >> stop)
    
-- |Observe a previous value of a signal.
-- Tipically used with rec statement.
cycleDelay ::
    ArrowApply a => ProcessA a b b
cycleDelay =
    encloseState impl (Nothing, Nothing)
  where
    impl :: ArrowApply a => ProcessA (StateArrow (Maybe b, Maybe b) a) b b
    impl = proc x ->
      do
        -- Load stored value when backtracking reaches here.
        (_, stored) <- peekState -< ()
        unsafeExhaust (app >>> arr (const Nothing)) -< appStore stored

        -- Repeat current value.
        (current, _) <- peekState -< ()
        let x0 = fromMaybe x current
        unsafeSteady store -< (Just x0, Just x)
        returnA -< x0

    appStore (Just x) = (proc _ -> store -< (Just x, Nothing), ())
    appStore _ = (Cat.id, ())
    
