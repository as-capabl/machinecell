{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module
    Control.Arrow.Machine.Plan
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
import Control.Arrow.Machine.Detail


data PlanF i o a where
  AwaitPF :: (i->a) -> PlanF i o a
  YieldPF :: o -> a -> PlanF i o a
  StopPF :: a -> PlanF i o a

instance (Functor (PlanF i o)) where
  fmap g (AwaitPF f) = AwaitPF (g . f)
  fmap g (YieldPF x r) = YieldPF x (g r)
  fmap g (StopPF r) = StopPF (g r)


type Plan i o m a = F.FreeT (PlanF i o) m a

yield :: Monad m => o -> Plan i o m ()
yield x = F.liftF $ YieldPF x ()

await_ :: Monad m => (i->Plan i o m a) -> Plan i o m a
await_ f = F.FreeT $ return $ F.Free $ AwaitPF f

await :: Monad m => Plan i o m i
await = await_ return

stop :: Monad m => Plan i o m ()
stop = F.liftF $ StopPF ()





construct :: Monad m => Plan i o m a -> 
             ProcessA (Kleisli m) (Event i) (Event o)

construct pl = ProcessA $ proc (ph, evx) ->
  do
    ff <- Kleisli (const $ F.runFreeT pl) -< ()
    go ph ff -<< evx

  where
    go Feed (F.Free (AwaitPF f)) = proc evx ->
      do
        (| hEv'
            (\x -> 
              do
                ff2 <- Kleisli (const $ F.runFreeT (f x)) -<< ()
                oneYieldPF Feed ff2 -<< ())
            (returnA -< (Feed, NoEvent, construct (await_ f)))
            (returnA -< (Feed, End, construct stop))
           |) evx

    go ph pfr = proc _ ->
        oneYieldPF ph pfr -<< ()

oneYieldPF :: Monad m => Phase -> 
              F.FreeF (PlanF i o) a (Plan i o m a) -> 
              Kleisli m () (Phase, 
                            Event o, 
                            ProcessA (Kleisli m) (Event i) (Event o))

oneYieldPF Suspend pfr = proc _ ->
    returnA -< (Suspend, NoEvent, construct $ F.FreeT $ return pfr)

oneYieldPF ph (F.Free (YieldPF x cont)) = proc _ ->
    returnA -< (Feed, Event x, construct cont)

oneYieldPF ph (F.Free (StopPF cont)) = proc _ ->
    returnA -< (ph `mappend` Suspend, End, construct stop)

oneYieldPF ph (F.Free pf) = proc _ ->
    returnA -< (ph `mappend` Suspend, 
                NoEvent, 
                construct $ F.FreeT $ return $ F.Free pf)

oneYieldPF ph (F.Pure x) = proc _ ->
    returnA -< (ph `mappend` Suspend, End, construct stop)


repeatedly :: Monad m => Plan i o m a -> ProcessA (Kleisli m) (Event i) (Event o)
repeatedly pl = construct $ forever pl
