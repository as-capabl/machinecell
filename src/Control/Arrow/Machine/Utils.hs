{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
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
        -- sampleR,
        -- sampleL,
        source,
        fork,
        filter,
        echo,
        anytime,
        par,
        parB,
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
import Control.Applicative
import Debug.Trace

import Control.Arrow.Machine.Types
import Control.Arrow.Machine.Event

import qualified Control.Arrow.Machine.Plan as Pl





delay :: 
    (ArrowApply a, Occasional b) => ProcessA a b b
delay = join >>> delayImpl >>> split
  where
    delayImpl = Pl.repeatedly $
      do
        x <- Pl.await
        Pl.yield noEvent
        Pl.yield x

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
accum old = proc evf ->
  do
    rSwitch (arr $ const old) -< ((), arr . const <$> (evf <*> pure old))

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

passRecent :: 
    (ArrowApply a, Occasional o) =>
    ProcessA a e (Event b) ->
    ProcessA a (e, b) o ->
    ProcessA a e o

passRecent af ag = proc e ->
  do
    evx <- af -< e
    mvx <- hold Nothing -< Just <$> evx
    case mvx of
      Just x -> ag -< (e, x)
      _ -> returnA -< noEvent

withRecent :: 
    (ArrowApply a, Occasional o) =>
    ProcessA a (e, b) o ->
    ProcessA a (e, Event b) o
withRecent af = proc (e, evb) ->
    (returnA -< evb) `passRecent` (\b -> af -< (e, b))


--
-- Switches
--
hEvPh :: ArrowApply a => a (e,b) c -> a e c -> a (e, (Phase, Event b)) c
hEvPh f1 f2 = proc (e, (ph, ev)) ->
    helper ph ev -<< e
  where
    helper Feed (Event x) = proc e -> f1 -< (e, x)
    helper _ _ = f2


hEvPh' :: ArrowApply a => a (e,b) c -> a e c -> a e c -> a (e, (Phase, Event b)) c
hEvPh' f1 f2 f3 = proc (e, (ph, ev)) ->
    helper ph ev -<< e
  where
    helper Feed (Event x) = proc e -> f1 -< (e, x)
    helper Feed End = f3
    helper _ _ = f2

switch :: 
    ArrowApply a => 
    ProcessA a b (c, Event t) -> 
    (t -> ProcessA a b c) ->
    ProcessA a b c

switch cur cont = ProcessA $ proc (ph, x) ->
  do
    (ph', (y, evt), new) <- step cur -< (ph, x)
    (| hEvPh
        (\t -> step (cont t) -<< (ph, x))
        (returnA -< (ph', y, switch new cont))
      |) 
        (ph', evt)


dSwitch :: 
    ArrowApply a => 
    ProcessA a b (c, Event t) -> 
    (t -> ProcessA a b c) ->
    ProcessA a b c

dSwitch cur cont = ProcessA $ proc (ph, x) ->
  do
    (ph', (y, evt), new) <- step cur -< (ph, x)
    
    returnA -< (ph', y, next new evt)
  where
    next _ (Event t) = cont t
    next new _ = dSwitch new cont


rSwitch :: 
    ArrowApply a => ProcessA a b c -> 
    ProcessA a (b, Event (ProcessA a b c)) c

rSwitch cur = ProcessA $ proc (ph, (x, eva)) -> 
  do
    (ph', y, new) <- 
        (| hEvPh
            (\af -> step af -<< (ph, x))
            (step cur -< (ph, x))
         |)
            (ph, eva)
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
    (| hEvPh
        (\t -> step $ k sf' t -<< (phT, x))
        (returnA -< (phT, y, kSwitch sf' test' k))
     |) 
        (phT, evt)


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

    (| hEvPh
       (\t -> step $ k sfs' t -<< (ph, x))
       (returnA -< (ph' `mappend` phT, zs, pSwitch r sfs' test' k))
     |) 
       (phT, evt)


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
    (ph', zs, sfs') <- parCore r sfs -<< (ph, x)

    (| hEvPh
       (\cont -> 
         do
           (ph'', ws, sfs'') <- parCore r (cont sfs') -<< (ph, x)
           returnA -< (ph'' `mappend` Suspend, ws, rpSwitch r sfs'')
         )
       (returnA -< (ph' `mappend` Suspend, zs, rpSwitch r sfs'))
     |) 
       (ph', evCont)


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

{-
-- Problem with the last output.
sampleR ::
    ArrowApply a =>
    ProcessA a (Event b1, Event b2) (Event (b1, [b2]))
sampleR = join >>> Pl.construct (go id)
  where
    go l = 
      do
        (evx, evy) <- Pl.await
        let l2 = evMaybe l (\y -> l . (y:)) evy
        evMaybe (go l2) (\x -> Pl.yield (x, l2 []) >> go id) evx

sampleL ::
    ArrowApply a =>
    ProcessA a (Event b1, Event b2) (Event ([b1], b2))
sampleL = arr swap >>> sampleR >>> evMap swap
-}

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


