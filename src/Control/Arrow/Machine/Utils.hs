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
        dHold,
        accum,
        dAccum,
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
        sample,
        source,
        fork,
        filter,
        echo,
        anytime,
        par,
        parB,
        now,
        onEnd
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
    
