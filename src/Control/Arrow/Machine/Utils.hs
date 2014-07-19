{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
        onEnd
       )
where

import Prelude hiding (filter)

import Data.Monoid (mappend, mconcat)
import Data.Tuple (swap)
import qualified Data.Foldable as Fd
import qualified Data.Traversable as Tv
import qualified Control.Category as Cat
import Control.Monad (liftM, forever)
import Control.Monad.Trans
import Control.Arrow
import Control.Arrow.Transformer.Reader (ReaderArrow, runReader)
import Control.Applicative
import Debug.Trace

import Control.Arrow.Machine.Types
import Control.Arrow.Machine.Event
import Control.Arrow.Machine.Event.Internal (Event(..))
import Control.Arrow.Machine.ArrowUtil

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

edge = ProcessA $ impl Nothing 
  where
    impl mvx = proc (ph, x) -> 
      do
        let equals = maybe False (==x) mvx
            isActive = not $ ph == Suspend
        returnA -< if (not equals) && isActive
          then 
            (Feed, Event x, ProcessA $ impl (Just x))
          else
            (ph `mappend` Suspend, NoEvent, ProcessA $ impl mvx)


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

--
-- other utility arrow
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
        (evx, evy) <- Pl.await
        let l2 = evMaybe l (\x -> l . (x:)) evx
        evMaybe (go l2) (\_ -> Pl.yield (l2 []) >> go id) evy

gather ::
    (ArrowApply a, Fd.Foldable f) =>
    ProcessA a (f (Event b)) (Event b)
gather = arr Event >>> 
    Pl.repeatedly 
        (Pl.await >>= Fd.mapM_ (evMaybe (return ()) Pl.yield))

-- |It's also possible that source is defined without any await.
-- 
-- But awaits are useful to synchronize other inputs.
source ::
    ArrowApply a =>
    [c] -> ProcessA a (Event b) (Event c)
source l = Pl.construct $ mapM_ yd l
  where
    yd x = Pl.await >> Pl.yield x

fork :: 
    (ArrowApply a, Fd.Foldable f) =>
    ProcessA a (Event (f b)) (Event b)

fork = Pl.repeatedly $ 
    Pl.await >>= Fd.mapM_ Pl.yield


anytime :: 
    ArrowApply a =>
    a b c ->
    ProcessA a (Event b) (Event c)

anytime action = Pl.repeatedlyT arrow $
  do
    x <- Pl.await
    ret <- lift $ (ArrowMonad $ arr (const x) >>> action)
    Pl.yield ret
  where
    arrow (ArrowMonad af) = af

{-
asNeeded action = ProcessA $ snd action >>> arr post
  where
    post (ph, y) = (ph `mconcat` Suspend, y, asNeeded action)

asNeeded :: 
    ArrowApply a =>
    a b Bool ->
    ProcessA a (Event b) (Event b)
-}

filter cond = Pl.repeatedlyT arrow $
  do
    x <- Pl.await
    b <- lift $ (ArrowMonad $ arr (const x) >>> cond)
    if b then Pl.yield x else return ()
  where
    arrow (ArrowMonad af) = af


echo :: 
    ArrowApply a =>
    ProcessA a (Event b) (Event b)

echo = filter (arr (const True))


onEnd ::
    (ArrowApply a, Occasional b) =>
    ProcessA a b (Event ())
{-
onEnd = dSwitch (arr go) id
  where
    go ev
        | isEnd ev = (undefined, Event Pl.stopped)
        | otherwise = noEvent
-}
onEnd = ProcessA $ proc (ph, ev) ->
  do
    returnA -< go ph ev
  where
    go ph ev 
        | isEnd ev = (Feed, Event (), Pl.stopped)
        | otherwise = (ph `mappend` Suspend, noEvent, onEnd)
