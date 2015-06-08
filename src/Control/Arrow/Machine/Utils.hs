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
        delay,
        hold,
        accum,
        edge,
        passRecent,
        withRecent,
        feedback1,
        feedback,

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

import Data.Monoid (mappend, mconcat)
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Foldable as Fd
import qualified Data.Traversable as Tv
import qualified Control.Category as Cat
import Control.Monad.Reader (ask)
import Control.Monad (liftM, forever)
import Control.Monad.Trans
import Control.Arrow
import Control.Arrow.Operations (ArrowState(..))
import Control.Arrow.Transformer.State (ArrowAddState(..), StateArrow())
import Control.Applicative
import Debug.Trace

import Control.Arrow.Machine.Types
import Control.Arrow.Machine.Event
import Control.Arrow.Machine.Event.Internal (Event(..))
import Control.Arrow.Machine.ArrowUtil
import Control.Arrow.Machine.Unsafe.Prim

import qualified Control.Arrow.Machine.Plan as Pl
import Control.Arrow.Machine.Exception



delay ::
    (ArrowApply a, Occasional b) => ProcessA a b b

delay = join >>> delayImpl >>> split
  where
    delayImpl = Pl.repeatedly $
      do
        mx <- liftM Just Pl.await `catch` return Nothing
        Pl.yield noEvent
        maybe Pl.stop Pl.yield mx


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
accum old = ProcessA $ proc (ph, evf) ->
  do
    let new = fromEvent id evf old
    returnA -< (ph `mappend` Suspend, new, accum new)

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
infixr 9 `feedback`

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


{-# DEPRECATED feedback1, feedback "Use Pump instead" #-}
-- |Event version of loop (member of `ArrowLoop`).             
-- Yielding an event to feedback output always creates a new process cycle.
-- So be careful to make an infinite loop.
feedback1 ::
    (ArrowApply a, Occasional d) =>
    ProcessA a (e, AS d) (c, d) ->
    ProcessA a (AS e) c
feedback1 pa = ProcessA $ proc (ph, ase) ->
  do
    (ph', (y, d), pa') <- step pa -< (ph, (fromAS ase, toAS noEvent))
    returnA -< (ph', y, cont ph' d pa')
  where
    cont phPrev d paC 
        | isOccasion d = ProcessA $ proc (ph, ase) ->
          do
            let 
              (dIn, dOut, phPv2, phCur) = 
                if ph == Suspend
                  then
                    (noEvent, const d, const phPrev, Suspend)
                  else
                    (d, id, id, ph `mappend` Feed)

            (ph', (y, d'), pa') <- step paC -< (phCur, (fromAS ase, toAS dIn))
            returnA -< (ph', y, cont (phPv2 ph') (dOut d') pa')

        | isEnd d && phPrev == Feed = ProcessA $ proc (ph, ase) ->
          do
            (ph', (y, _), pa') <- step paC -< (ph, (fromAS ase, toAS end))
            returnA -< (ph', y, proc asx -> arr fst <<< pa' -< (fromAS asx, toAS end))

        | otherwise = feedback1 paC


-- |Artificially split into two arrow to use binary operator notation
-- rather than banana brackets.
feedback ::
    (ArrowApply a, Occasional d) =>
    ProcessA a (e, AS d) b ->
    ProcessA a (e, AS b) (c, d) ->
    ProcessA a (AS e) c
feedback pa pb = 
    feedback1 $ proc (ase, x) -> 
      do 
        y <- pa -< (ase, x)
        pb -< (ase, toAS y)


--
-- Switches
--
evMaybePh :: b -> (a->b) -> (Phase, Event a) -> b
evMaybePh _ f (Feed, Event x) = f x
evMaybePh _ f (Sweep, Event x) = f x
evMaybePh d _ _ = d


switchCore sw cur cont = sw cur (arr test) cont' >>> arr fst
  where
    test (_, (_, evt)) = evt
    cont' _ t = cont t >>> arr (\y -> (y, noEvent))

switch :: 
    ArrowApply a => 
    ProcessA a b (c, Event t) -> 
    (t -> ProcessA a b c) ->
    ProcessA a b c

switch = switchCore kSwitch


dSwitch :: 
    ArrowApply a => 
    ProcessA a b (c, Event t) -> 
    (t -> ProcessA a b c) ->
    ProcessA a b c

dSwitch = switchCore dkSwitch


rSwitch :: 
    ArrowApply a => ProcessA a b c -> 
    ProcessA a (b, Event (ProcessA a b c)) c

rSwitch cur = ProcessA $ proc (ph, (x, eva)) -> 
  do
    let now = evMaybePh cur id (ph, eva)
    (ph', y, new) <-  step now -<< (ph, x)
    returnA -< (ph', y, rSwitch new)


drSwitch :: 
    ArrowApply a => ProcessA a b c -> 
    ProcessA a (b, Event (ProcessA a b c)) c

drSwitch cur = ProcessA $ proc (ph, (x, eva)) -> 
  do
    (ph', y, new) <- step cur -< (ph, x)
    
    returnA -< (ph', y, next new eva)

  where
    next _ (Event af) = drSwitch af
    next af _ = drSwitch af


kSwitch ::
    ArrowApply a => 
    ProcessA a b c ->
    ProcessA a (b, c) (Event t) ->
    (ProcessA a b c -> t -> ProcessA a b c) ->
    ProcessA a b c

kSwitch sf test k = ProcessA $ proc (ph, x) ->
  do
    (ph', y, sf') <- step sf -< (ph, x)
    (phT, evt, test') <- step test -< (ph', (x, y))

    evMaybePh 
        (arr $ const (phT, y, kSwitch sf' test' k)) 
        (step . (k sf'))
        (phT, evt)
            -<< (phT, x)


dkSwitch ::
    ArrowApply a => 
    ProcessA a b c ->
    ProcessA a (b, c) (Event t) ->
    (ProcessA a b c -> t -> ProcessA a b c) ->
    ProcessA a b c

dkSwitch sf test k = ProcessA $ proc (ph, x) ->
  do
    (ph', y, sf') <- step sf -< (ph, x)
    (phT, evt, test') <- step test -< (ph', (x, y))
    
    let
        nextA t = k sf' t
        nextB = dkSwitch sf' test' k

    returnA -< (phT, y, evMaybe nextB nextA evt)


broadcast :: 
    Functor col =>
    b -> col sf -> col (b, sf)

broadcast x sfs = fmap (\sf -> (x, sf)) sfs


par ::
    (ArrowApply a, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessA a ext c) ->
    ProcessA a b (col c)

par r sfs = ProcessA $ parCore r sfs >>> arr cont
  where
    cont (ph, ys, sfs') = (ph, ys, par r sfs')

parB ::
    (ArrowApply a, Tv.Traversable col) =>
    col (ProcessA a b c) ->
    ProcessA a b (col c)

parB = par broadcast

parCore ::
    (ArrowApply a, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessA a ext c) ->
    a (Phase, b) (Phase, col c, col (ProcessA a ext c))

parCore r sfs = proc (ph, x) ->
  do
    let input = r x sfs

    ret <- unwrapArrow (Tv.sequenceA (fmap (WrapArrow . appPh) input)) -<< ph

    let ph' = Fd.foldMap getPh ret
        zs = fmap getZ ret
        sfs' = fmap getSf ret

    returnA -< (ph', zs, sfs')

  where
    appPh (y, sf) = proc ph -> step sf -< (ph, y)

    getPh (ph, _, _) = ph
    getZ (_, z, _) = z
    getSf (_, _, sf) = sf


pSwitch ::
    (ArrowApply a, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessA a ext c) ->
    ProcessA a (b, col c) (Event mng) ->
    (col (ProcessA a ext c) -> mng -> ProcessA a b (col c)) ->
    ProcessA a b (col c)

pSwitch r sfs test k = ProcessA $ proc (ph, x) ->
  do
    (ph', zs, sfs') <- parCore r sfs -<< (ph, x)
    (phT, evt, test') <- step test -< (ph', (x, zs))

    evMaybePh
        (arr $ const (ph' `mappend` phT, zs, pSwitch r sfs' test' k))
        (step . (k sfs') )
        (phT, evt)
            -<< (ph, x)

pSwitchB ::
    (ArrowApply a, Tv.Traversable col) =>
    col (ProcessA a b c) ->
    ProcessA a (b, col c) (Event mng) ->
    (col (ProcessA a b c) -> mng -> ProcessA a b (col c)) ->
    ProcessA a b (col c)

pSwitchB = pSwitch broadcast


rpSwitch ::
    (ArrowApply a, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessA a ext c) ->
    ProcessA a (b, Event (col (ProcessA a ext c) -> col (ProcessA a ext c)))
        (col c)

rpSwitch r sfs = ProcessA $ proc (ph, (x, evCont)) ->
  do
    let sfsNew = evMaybePh sfs ($sfs) (ph, evCont)
    (ph', ws, sfs') <- parCore r sfsNew -<< (ph, x)
    returnA -< (ph' `mappend` Suspend, ws, rpSwitch r sfs')


rpSwitchB ::
    (ArrowApply a, Tv.Traversable col) =>
    col (ProcessA a b c) ->
    ProcessA a (b, Event (col (ProcessA a b c) -> col (ProcessA a b c)))
        (col c)

rpSwitchB = rpSwitch broadcast

-- `dpSwitch` and `drpSwitch` are not implemented.


--
-- State arrow
--
peekState ::
    (ArrowApply a, ArrowState s a) =>
    ProcessA a e s
peekState = unsafeSteady fetch

encloseState ::
    (ArrowApply a, ArrowAddState s a a') =>
    ProcessA a b c ->
    s ->
    ProcessA a' b c
encloseState pa s = ProcessA $ proc (ph, x) ->
  do
    ((ph', y, pa'), s') <- elimState (step pa) -< ((ph, x), s)
    returnA -< (ph', y, encloseState pa' s')

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
tee = join >>> go
  where
    go = Pl.repeatedly $ 
      do
        (evx, evy) <- Pl.await
        evMaybe (return ()) (Pl.yield . Left) evx
        evMaybe (return ()) (Pl.yield . Right) evy


sample ::
    ArrowApply a =>
    ProcessA a (Event b1, Event b2) [b1]
sample = join >>> Pl.construct (go id) >>> hold []
  where
    go l = 
      do
        (evx, evy) <- Pl.await `catch` return (NoEvent, End)
        let l2 = evMaybe l (\x -> l . (x:)) evx
        if isEnd evy
          then
          do
            Pl.yield $ l2 []
            Pl.stop
          else
            return ()
        evMaybe (go l2) (\_ -> Pl.yield (l2 []) >> go id) evy

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
source l = Pl.construct $ Fd.mapM_ yd l
  where
    yd x = Pl.await >> Pl.yield x

-- |Given an array-valued event and emit it's values as inidvidual events.
fork ::
    (ArrowApply a, Fd.Foldable f) =>
    ProcessA a (Event (f b)) (Event b)

fork = Pl.repeatedly $ 
    Pl.await >>= Fd.mapM_ Pl.yield

-- |Executes an action once per an input event is provided.
anytime :: 
    ArrowApply a =>
    a b c ->
    ProcessA a (Event b) (Event c)

anytime action = Pl.repeatedlyT (ary0 unArrowMonad) $
  do
    x <- Pl.await
    ret <- lift $ arrowMonad action x
    Pl.yield ret


filter cond = Pl.repeatedlyT (ary0 unArrowMonad) $
  do
    x <- Pl.await
    b <- lift $ arrowMonad cond x
    if b then Pl.yield x else return ()


echo :: 
    ArrowApply a =>
    ProcessA a (Event b) (Event b)

echo = filter (arr (const True))

now ::
    ArrowApply a =>
    ProcessA a b (Event ())
now = arr (const noEvent) >>> go
  where
    go = Pl.construct $
        Pl.yield () >> forever Pl.await

onEnd ::
    (ArrowApply a, Occasional b) =>
    ProcessA a b (Event ())
onEnd = arr collapse >>> go
  where
    go = Pl.repeatedly $
        Pl.await `catch` (Pl.yield () >> Pl.stop)
    
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
    
