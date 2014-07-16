{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
A coroutine monad, inspired by machines library.
-}
module
    Control.Arrow.Machine.Plan
      (
        -- * Types and Primitives
        PlanT,
        Plan,

        await,
        yield,
        stop,

        stopped,

        -- * Constructing machines
        constructT,
        repeatedlyT,

        construct,
        repeatedly
       )
where

import qualified Control.Category as Cat
import qualified Control.Monad.Trans.Free as F

import Data.Monoid (mappend)
import Data.Functor ((<$>))
import Control.Monad
import Control.Arrow
import Control.Monad.Trans
import Debug.Trace

import Control.Arrow.Machine.Types
import Control.Arrow.Machine.Event
import Control.Arrow.Machine.Event.Internal (Event(..))

import Control.Arrow.Machine.Plan.Internal

stopped :: 
    (ArrowApply a, Occasional c) => ProcessA a b c
stopped = arr (const end)





yield :: o -> Plan i o ()
yield x = F.liftF $ YieldPF x ()

await_ :: Monad m => (i->PlanT i o m a) -> PlanT i o m a
await_ f = F.FreeT $ return $ F.Free $ (AwaitPF f stop)

await :: Plan i o i
await = await_ return

stop :: Plan i o a
stop = F.liftF $ StopPF






constructT :: (Monad m, ArrowApply a) => 
              (forall b. m b -> a () b) ->
              PlanT i o m r -> 
              ProcessA a (Event i) (Event o)

constructT fit pl = ProcessA $ proc (ph, evx) ->
  do
    probe ph pl -<< evx
    
  where
    runAndYield fx = proc _ ->
      do
        ff2 <- fit (F.runFreeT fx) -<< ()
        oneYieldPF fit Feed ff2 -<< ()

    probe Suspend pl = proc _ ->
        returnA -< (Suspend, NoEvent, constructT fit pl)
        
    probe ph pl = proc evx ->
      do
        pfr <- fit (F.runFreeT pl) -< ()
        go ph pfr -<< evx

    go Feed (F.Free (AwaitPF f ff)) = arr (\evx -> ((), evx)) >>>
        hEv' (proc (_, x) -> runAndYield (f x) -<< ()) 
             (arr $ const (Feed, NoEvent, constructT fit (await_ f))) 
             (proc _ -> runAndYield ff -<< ())

    go ph pfr = proc evx ->
      do
        let action = case (evx, pfr) of {(End, F.Free (AwaitPF _ ff)) -> ff; _ -> F.FreeT $ return pfr}
        pfr' <- fit (F.runFreeT action) -<< ()
        oneYieldPF fit ph pfr' -<< ()


oneYieldPF :: (Monad m, ArrowApply a) => 
              (forall b. m b -> a () b) ->
              Phase -> 
              F.FreeF (PlanF i o) r (PlanT i o m r) -> 
              a () (Phase, 
                    Event o, 
                    ProcessA a (Event i) (Event o))

oneYieldPF f Suspend pfr = proc _ ->
    returnA -< (Suspend, NoEvent, constructT f $ F.FreeT $ return pfr)

oneYieldPF f ph (F.Free (YieldPF x cont)) = proc _ ->
    returnA -< (Feed, Event x, constructT f cont)

oneYieldPF f ph (F.Free StopPF) = proc _ ->
    returnA -< (ph `mappend` Suspend, End, stopped)

oneYieldPF f ph (F.Free pf) = proc _ ->
    returnA -< (ph `mappend` Suspend, 
                NoEvent, 
                constructT f $ F.FreeT $ return $ F.Free pf)

oneYieldPF f ph (F.Pure x) = proc _ ->
    returnA -< (ph `mappend` Suspend, End, stopped)


repeatedlyT :: (Monad m, ArrowApply a) => 
              (forall b. m b -> a () b) ->
              PlanT i o m r -> 
              ProcessA a (Event i) (Event o)

repeatedlyT f pl = constructT f $ forever pl


-- for pure
construct :: ArrowApply a =>
             Plan i o t -> 
             ProcessA a (Event i) (Event o)
construct pl = constructT kleisli pl
  where
    kleisli (ArrowMonad a) = a
{-
    unKleisli (Kleisli f) = proc x -> 
        case f x of {ArrowMonad af -> af} -<< ()
-}    

repeatedly :: ArrowApply a =>
              Plan i o t -> 
              ProcessA a (Event i) (Event o)
repeatedly pl = construct $ forever pl
