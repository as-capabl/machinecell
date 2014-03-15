{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
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



data ProcessA a b c = ProcessA { 
      step :: a (Phase, b) (Phase, c, ProcessA a b c) 
    }


toProcessA :: ArrowApply a => 
              Mc.Machine (a b) c -> 
              ProcessA a (Event b) (Event c)
toProcessA mc = ProcessA $ proc (ph, x) ->
  do
    mcStep ph mc -<< x

mcStep :: ArrowApply a => Phase -> Mc.Machine (a b) c -> 
          a (Event b) (Phase, Event c, ProcessA a (Event b) (Event c))    
mcStep Feed mc@(Mc.Await fc f ff) = proc evx ->
  do
    (| hEv' 
        (\x -> 
          do
            y <- f -< x
            oneYield Feed (fc y) -<< ())
        (returnA -< (Feed, NoEvent, toProcessA mc))
        (oneYield Feed ff -<< ())
      |) evx

mcStep ph mc = proc _ ->
  do
    oneYield ph mc -<< ()


oneYield :: ArrowApply a => Phase -> 
            Mc.Machine (a b) c -> 
            a () (Phase, Event c, ProcessA a (Event b) (Event c))    

oneYield Suspend mc = proc _ ->
    returnA -< (Suspend, NoEvent, toProcessA mc)

oneYield ph (Mc.Yield y fc) = proc _ ->
    returnA -< (Feed, Event y, toProcessA fc)

oneYield ph Mc.Stop = proc _ ->
    returnA -< (ph `mappend` Suspend, End, toProcessA Mc.Stop)

oneYield ph mc = proc _ ->
    returnA -< (ph `mappend` Suspend, NoEvent, toProcessA mc)


instance
    ArrowApply a => Cat.Category (ProcessA a)
  where
    id = ProcessA (arrStep id)
    g . f = ProcessA $ proc (ph, x) -> concatStep ph f g -<< x

instance 
    ArrowApply a => Arrow (ProcessA a)
  where
    arr = ProcessA . arrStep

    first (ProcessA a) = ProcessA $ proc (ph, (x, d)) ->
      do
        (ph', y, pa') <- a -< (ph, x)
        returnA -< (ph' `mappend` Suspend, (y, d), first pa')

arrStep :: ArrowApply a => (b->c) -> a (Phase, b) (Phase, c, ProcessA a b c)
arrStep f = proc (ph, x) ->
    returnA -< (ph `mappend` Suspend, f x, ProcessA $ arrStep f)

concatStep :: ArrowApply a => 
              Phase -> 
              ProcessA a b d -> ProcessA a d c -> 
              a b (Phase, c, ProcessA a b c)
concatStep Sweep pa pb = proc x ->
  do
    (ph1, r1, pa') <- step pa -< (Suspend, x)
    (ph2, r2, pb') <- step pb -<< (Sweep, r1)
    cont ph2 x -<< (ph2, r2, ProcessA $ proc (ph, x) -> concatStep ph pa' pb' -<< x)
  where
    cont Feed x = returnA
    cont Sweep x = returnA
    cont Suspend x = proc _ ->
      do
        (ph1, r1, pa') <- step pa -< (Sweep, x)
        (ph2, r2, pb') <- step pb -< (ph1, r1)
        returnA -< (ph2, r2, ProcessA $ proc (ph, x) -> concatStep ph pa' pb' -<< x)

concatStep ph pa pb = proc x ->
  do
    (ph1, r1, pa') <- step pa -< (ph, x)
    (ph2, r2, pb') <- step pb -<< (ph1, r1)
    returnA -< (ph2, r2, ProcessA $ proc (ph, x) -> concatStep ph pa' pb' -<< x)


runProcessA :: ArrowApply a => ProcessA a (Event b) (Event c) -> a [b] [c]
runProcessA pa = proc xs -> 
  do
    ys <- go Sweep pa xs id -<< ()
    returnA -< ys []
  where
    go Sweep pa [] ys = proc _ ->
      do
        (ph', y, pa') <- step pa -< (Sweep, End)
        react y ph' pa' [] ys -<< ()

    go Feed pa [] ys = arr $ const ys

    go ph pa (x:xs) ys = proc _ ->
      do
        let (evx, xs') = if ph == Feed then (Event x, xs) else (NoEvent, x:xs)
        (ph', y, pa') <- step pa -< (ph, evx)
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
    left pa@(ProcessA a) = ProcessA $ proc (ph, eth) -> go ph eth -<< ()
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
    loop pa = ProcessA $ proc (ph, x) -> loop $ go ph -<< x
      where
        go ph = proc (x, d) ->
          do 
            (ph', (y, d'), pa') <- step pa -< (ph, (x, d))
            returnA -< ((ph', y, loop pa'), d')
