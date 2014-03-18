{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module
    Control.Arrow.Machine.Utils
where

import Prelude hiding (filter)

import Data.Monoid (mappend)
import qualified Control.Category as Cat
import Control.Monad (liftM, mzero, forever)
import Control.Monad.Trans
import Control.Arrow
import Control.Applicative
import Debug.Trace

import Control.Arrow.Machine.Types
import Control.Arrow.Machine.Event
import Control.Arrow.Machine.Detail

import qualified Control.Arrow.Machine.Plan as Pl



kleisli :: ArrowApply a => a b c -> Kleisli (ArrowMonad a) b c
kleisli af = Kleisli $ \x -> ArrowMonad $ arr (const x) >>> af

unKleisli :: ArrowApply a => Kleisli (ArrowMonad a) b c -> a b c
unKleisli (Kleisli f) = proc x -> case f x of {ArrowMonad af -> af} -<< ()


wye :: ArrowApply a => ProcessA a (Event b1, Event b2) (Event (Either b1 b2))
wye = join >>> fit unKleisli (Pl.repeatedly pl)
  where
    pl =
      do
        (evx, evy) <- Pl.await
        evMaybe (return ()) (Pl.yield . Left) evx
        evMaybe (return ()) (Pl.yield . Right) evy

delay :: (ArrowApply a, Occasional b) => ProcessA a b b
delay = join >>> fit unKleisli delayImpl >>> split
  where
    delayImpl = Pl.repeatedly $
      do
        x <- Pl.await
        Pl.yield noEvent
        Pl.yield x

hold :: ArrowApply a => b -> ProcessA a (Event b) b
hold old = ProcessA $ proc (ph, evx) ->
  do
    let new = fromEvent old evx
    returnA -< (ph `mappend` Suspend, new, hold new)

var :: (ArrowApply a, ArrowLoop a) => b -> ProcessA a (Event (b -> b)) b
var i = proc f -> 
  do
    rec x <- hold i <<< delay -< f <*> Event x
    returnA -< x

edge :: (ArrowApply a, Eq b) =>
         ProcessA a b (Event b)
edge = ProcessA $ impl Nothing 
  where
    impl mvx = proc (ph, x) -> 
      do
        let equals = maybe False (==x) mvx
        returnA -< if not equals 
          then 
            (Feed, Event x, ProcessA $ impl (Just x))
          else
            (ph `mappend` Suspend, NoEvent, ProcessA $ impl mvx)
    

fork :: (ArrowApply a) =>
        ProcessA a (Event [b]) (Event b)
fork = fit unKleisli $ Pl.repeatedly $ 
    Pl.await >>= mapM Pl.yield

{-
once :: ArrowApply a =>
        a b c ->
        ProcessA a (Event b) (Maybe c)
once action = toProcessA go >>> hold Nothing
  where
    go = Mc.construct $
      do
        ret <- Mc.request action
        Mc.yield $ Just ret
        forever $ Mc.request (arr id)
-}

anyTime :: ArrowApply a =>
        a b c ->
        ProcessA a (Event b) (Event c)
anyTime action = fit unKleisli go
  where
    go = Pl.repeatedly $
      do
        x <- Pl.await
        ret <- lift $ (runKleisli $ kleisli action) x
        Pl.yield ret


filter :: ArrowApply a =>
          a b Bool ->
          ProcessA a (Event b) (Event b)
filter cond = fit unKleisli $ Pl.repeatedly $
  do
    x <- Pl.await
    b <- lift $ (runKleisli $ kleisli cond) x
    if b then Pl.yield x else return ()


pass :: ArrowApply a =>
          ProcessA a (Event b) (Event b)
pass = filter (arr (const True))

{-
accumulate :: (ArrowApply a, ArrowLoop a) => 
              (c->b->c) -> c -> ProcessA a (Event b) c
accumulate f init = proc evx -> 
  do
    rec
      current <- hold init <<< delay -< next
      next <- returnA -< f current `fmap` evx
    returnA -< current
-}

--
-- ローカルBehavior
--
infixr 9 `passRecent`

passRecent :: (ArrowApply a, Occasional o) =>
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

withRecent :: (ArrowApply a, Occasional o) =>
              ProcessA a (e, b) o ->
              ProcessA a (e, Event b) o
withRecent af = proc (e, evb) ->
    (returnA -< evb) `passRecent` (\b -> af -< (e, b))

{-
variable :: (ArrowApply a, ArrowLoop a, Occasional o) =>
            ProcessA a e b ->
            ProcessA a (e, b) (o, Event b) ->
            ProcessA a e o

variable af ag = proc e ->
  do
    binit <- af -< e
    rec 
        (o, bnew) <-(| withRecent (\b -> ag -< (e, b)) |) bdly
        bdly <- (once -< Event binit) `successive` (delay -< bnew)
    returnA -< o

once :: ArrowApply a => ProcessA a (Event b) (Event b)
once = toProcessA $ Mc.construct $ awaitA >>= Mc.yield

successive :: ArrowApply a => 
              ProcessA a e (Event o) ->
              ProcessA a e (Event o) ->
              ProcessA a e (Event o)
successive af ag = proc e ->
  do
    x <- af -< e
    if isEnd x
      then
        ag -< e
      else
        returnA -< x
-}
---
--- ステップ実行
---
type Running a b c = ProcessA a b c

startRun :: Arrow a =>
             ProcessA a (Event b) (Event c) ->
             Running a (Event b) (Event c)
startRun = id

stepRun :: ArrowApply a =>
            Running a (Event b) (Event c) ->
            a b ([c], Running a (Event b) (Event c))
stepRun pa = proc x ->
  do
    (ys, pa') <- go Feed pa (Event x) id -<< ()
    returnA -< (ys [], pa')
  where
    go Suspend pa _ ys = proc _ ->
        returnA -< (ys, pa)

    go ph pa evx ys = proc _ ->
      do
        (ph', y, pa') <- step pa -< (ph, evx)
        react y ph' pa' ys -<< ()
    
    react End ph pa ys =
      do
        go (adv ph) pa End ys

    react (Event y) ph pa ys =
        go (adv ph) pa NoEvent (\cont -> ys (y:cont))

    react NoEvent ph pa ys =
        go (adv ph) pa NoEvent ys

    adv Feed = Sweep
    adv Suspend = Suspend

{-
isStop :: ProcessA_ a b c -> Bool
isStop (ProcessA_ pre post Mc.Stop) = True
isStop _ = False
-}
