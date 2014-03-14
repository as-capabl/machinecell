{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module
    Control.Arrow.Machine.Plan
where

import qualified Data.Machine as Mc
import qualified Control.Category as Cat

import qualified Control.Monad.Free as F

import Control.Monad
import Control.Arrow
import Control.Applicative
import Control.Monad.Trans
import Debug.Trace

import Control.Arrow.Machine.Types
import Control.Arrow.Machine.Event
import Control.Arrow.Machine.Detail


data PlanF i o m a where
  AwaitPF :: (i -> m a) -> PlanF i o m a
  YieldPF :: o -> a -> PlanF i o m a
  StopPF :: a -> PlanF i o m a

instance (Monad m => Functor (PlanF i o m)) where
  fmap g (AwaitPF fm) = AwaitPF (\x -> fm x >>= return . g)
  fmap g (YieldPF x r) = YieldPF x (g r)
  fmap g (StopPF r) = StopPF (g r)


type Plan i o m a = F.Free (PlanF i o m) a

yield :: o -> Plan i o m ()
yield x = F.Free $ YieldPF x (F.Pure ())

awaits :: Monad m => (i -> m a) -> Plan i o m a
awaits fmx = F.Free $ fmap F.Pure $ AwaitPF fmx

awaitDo :: Monad m => m a -> Plan i o m a
awaitDo = awaits . const

await :: Monad m => Plan i o m i
await = awaits return

stop :: Plan i o m ()
stop = F.Free $ StopPF (F.Pure ())





construct :: Monad m => Plan i o m a -> ProcessA (Kleisli m) (Event i) (Event o)
construct = toProcessA . constructMc

constructMc (F.Pure x) = Mc.Stop

constructMc (F.Free (AwaitPF fm)) = 
    Mc.Await constructMc (Kleisli fm) Mc.Stop

constructMc (F.Free (YieldPF x cont)) = Mc.Yield x $ constructMc cont

-- constructMc (F.Free (StopPF cont)) = constructMc cont
constructMc (F.Free (StopPF cont)) = Mc.Stop





repeatedly :: Monad m => Plan i o m a -> ProcessA (Kleisli m) (Event i) (Event o)
repeatedly pl = construct $ repeatIt pl pl

repeatIt orig (F.Pure x) = repeatIt orig orig

repeatIt orig (F.Free pf) = F.Free $ fmap (repeatIt orig) pf



