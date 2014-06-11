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
import Control.Monad
import Control.Arrow
import Control.Applicative
import Control.Monad.Trans
import Debug.Trace

import Control.Arrow.Machine.Types
import Control.Arrow.Machine.Event


stopped :: 
    (ArrowApply a, Occasional c) => ProcessA a b c
stopped = arr (const end)



data PlanF i o a where
  AwaitPF :: (i->a) -> PlanF i o a
  YieldPF :: o -> a -> PlanF i o a
  StopPF :: a -> PlanF i o a

instance (Functor (PlanF i o)) where
  fmap g (AwaitPF f) = AwaitPF (g . f)
  fmap g (YieldPF x r) = YieldPF x (g r)
  fmap g (StopPF r) = StopPF (g r)


type PlanT i o m a = F.FreeT (PlanF i o) m a
type Plan i o a = forall m. Monad m => PlanT i o m a


yield :: o -> Plan i o ()
yield x = F.liftF $ YieldPF x ()

await_ :: Monad m => (i->PlanT i o m a) -> PlanT i o m a
await_ f = F.FreeT $ return $ F.Free $ AwaitPF f

await :: Plan i o i
await = await_ return

stop :: Plan i o ()
stop = F.liftF $ StopPF ()





constructT :: (Monad m, ArrowApply a) => 
              (forall b. m b -> a () b) ->
              PlanT i o m r -> 
              ProcessA a (Event i) (Event o)

constructT fit pl = ProcessA $ proc (ph, evx) ->
  do
    probe ph pl -<< evx
    

  where
    probe Suspend pl = proc _ ->
        returnA -< (Suspend, NoEvent, constructT fit pl)
        
    probe ph pl = proc evx ->
      do
        ff <- fit (F.runFreeT pl) -< ()
        go ph ff -<< evx


    go Feed (F.Free (AwaitPF f)) = proc evx ->
      do
        (| hEv'
            (\x -> 
              do
                ff2 <- fit (F.runFreeT (f x)) -<< ()
                oneYieldPF fit Feed ff2 -<< ())
            (returnA -< (Feed, NoEvent, constructT fit (await_ f)))
            (returnA -< (Feed, End, stopped))
           |) evx

    go ph pfr = proc evx ->        
        oneYieldPF fit ph pfr -<< ()

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

oneYieldPF f ph (F.Free (StopPF cont)) = proc _ ->
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
