{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module
    Control.Arrow.Machine.Types
where

import qualified Data.Machine as Mc
import qualified Control.Category as Cat
import Data.Monoid
import Control.Monad (liftM)
import Control.Arrow
import Control.Applicative
import Control.Monad.Trans
import Debug.Trace
import Data.Maybe

import Control.Arrow.Machine.Event
import Control.Arrow.Machine.Detail


data Phase = Feed | Sweep | Suspend deriving (Eq, Show)

instance 
    Monoid Phase 
  where
    mempty = Sweep
    mappend Feed _ = Feed
    mappend _ Feed = Feed
    mappend Suspend _ = Suspend
    mappend _ Suspend = Suspend
    mappend Sweep Sweep = Sweep



data
    ProcessA a b c
  where
    General :: a (Phase, b) (Phase, c, ProcessA a b c) -> ProcessA a b c


toProcessA :: ArrowApply a => Mc.Machine (a b) c -> ProcessA a (Event b) (Event c)
toProcessA mc = General $ proc (ph, x) ->
  do
    stepMc ph mc -<< x

stepMc :: ArrowApply a => Phase -> Mc.Machine (a b) c -> a (Event b) (Phase, Event c, ProcessA a (Event b) (Event c))    
stepMc Feed mc@(Mc.Await fc f ff) = proc evx ->
  do
    (| hEv' 
        (\x -> 
          do
            y <- f -< x
            oneYieldMc Feed (fc y) -<< ())
        (returnA -< (Suspend, NoEvent, toProcessA mc))
        (oneYieldMc Suspend ff -<< ())
      |) evx

stepMc Feed Mc.Stop = proc evx ->
  do
    returnA -< (Feed, End, toProcessA Mc.Stop)

stepMc ph mc = proc _ ->
  do
    oneYieldMc ph mc -<< ()


oneYieldMc :: ArrowApply a => Phase -> 
              Mc.Machine (a b) c -> a () (Phase, Event c, ProcessA a (Event b) (Event c))    

oneYieldMc Suspend mc = proc _ ->
    returnA -< (Suspend, NoEvent, toProcessA mc)

oneYieldMc ph (Mc.Yield y fc) = proc _ ->
    returnA -< (Feed, Event y, toProcessA fc)

oneYieldMc ph Mc.Stop = proc _ ->
    returnA -< (Suspend, End, toProcessA Mc.Stop)

oneYieldMc ph mc = proc _ ->
    returnA -< (Suspend, NoEvent, toProcessA mc)


instance
    ArrowApply a => Cat.Category (ProcessA a)
  where
    id = General (arrStep id)
    g . f = General $ proc (ph, x) -> concatStep ph f g -<< x

instance 
    ArrowApply a => Arrow (ProcessA a)
  where
    arr = General . arrStep

    first (General a) = General $ proc (ph, (x, d)) ->
      do
        (ph', y, pa') <- a -< (ph, x)
        returnA -< (ph' `mappend` Suspend, (y, d), first pa')

arrStep :: ArrowApply a => (b->c) -> a (Phase, b) (Phase, c, ProcessA a b c)
arrStep f = proc (ph, x) ->
    returnA -< (ph `mappend` Suspend, f x, General $ arrStep f)

concatStep :: ArrowApply a => Phase -> ProcessA a b d -> ProcessA a d c -> a b (Phase, c, ProcessA a b c)
concatStep Sweep pa pb = proc x ->
  do
    (r1, pa', ph1) <- step Suspend pa -< x
    (r2, pb', ph2) <- step Sweep pb -<< r1
    cont ph2 x -<< (ph2, r2, General $ proc (ph, x) -> concatStep ph pa' pb' -<< x)
  where
    cont Feed x = returnA
    cont Sweep x = returnA
    cont Suspend x = proc _ ->
      do
        (r1, pa', ph1) <- step Sweep pa -< x
        (r2, pb', ph2) <- step ph1 pb -<< r1
        returnA -< (ph2, r2, General $ proc (ph, x) -> concatStep ph pa' pb' -<< x)

concatStep ph pa pb = proc x ->
  do
    (r1, pa', ph1) <- step ph pa -< x
    (r2, pb', ph2) <- step ph1 pb -<< r1
    returnA -< (ph2, r2, General $ proc (ph, x) -> concatStep ph pa' pb' -<< x)


step :: ArrowApply a => Phase -> ProcessA a b c -> a b (c, ProcessA a b c, Phase)

step ph (General a) = proc x ->
  do
    (ph', r, pa') <- a -< (ph, x)
    returnA -< (r, pa', ph')


runProcessA :: ArrowApply a => ProcessA a (Event b) (Event c) -> a [b] [c]
runProcessA pa = proc xs -> 
  do
    ys <- go Sweep pa xs id -<< ()
    returnA -< ys []
  where
    go Sweep pa [] ys = proc _ ->
      do
        (y, pa', ex') <- step Sweep pa -< End
        react y ex' pa' [] ys -<< ()

    go Feed pa [] ys = arr $ const ys

    go ph pa (x:xs) ys = proc _ ->
      do
        let (evx, xs') = if ph == Feed then (Event x, xs) else (NoEvent, x:xs)
        (y, pa', ph') <- step ph pa -< evx
        react y ph' pa' xs' ys -<< ()
    
    react End ph pa xs ys =
      do
        go (adv ph) pa [] ys

    react (Event y) ph pa xs ys =
        go (adv ph) pa xs (\cont -> ys (y:cont))

    react NoEvent ph pa xs ys =
        go (adv ph) pa xs ys

    adv Feed = Sweep
    adv Suspend = Feed


instance
    ArrowApply a => ArrowChoice (ProcessA a)
  where
    left pa@(General a) = General $ proc (ph, eth) -> go ph eth -<< ()
      where
        go ph (Left x) = proc _ -> 
          do
            (ph', y, pa') <- a -< (ph, x)
            returnA -< (ph', Left y, left pa')
        go ph (Right d) = proc _ -> 
            returnA -< (ph `mappend` Suspend, Right d, left pa)

instance
    (ArrowApply a, ArrowLoop a) => ArrowLoop (ProcessA a)
  where
    loop pa = General $ proc (ph, x) -> loop $ go ph -<< x
      where
        go ph = proc (x, d) ->
          do 
            ((y, d'), pa', ph') <- step ph pa -< (x, d)
            returnA -< ((ph', y, loop pa'), d')
