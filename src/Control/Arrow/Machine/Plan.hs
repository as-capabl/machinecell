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
import qualified Control.Monad.Trans.Free.Church as F

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

await :: Plan i o i
await = F.FT $ \pure free -> free (AwaitPF pure (free StopPF))

stop :: Plan i o a
stop = F.liftF $ StopPF






constructT :: (Monad m, ArrowApply a) => 
              (forall b. m b -> a () b) ->
              PlanT i o m r -> 
              ProcessA a (Event i) (Event o)

constructT fit pl = ProcessA $ fit' $ F.runFT pl pure free
  where
    fit' ma = proc arg -> do { (evx, pa) <- fit ma -< (); modFit evx pa -<< arg }
    
    modFit :: ArrowApply a => Event c -> StepType a b (Event c) -> StepType a b (Event c)
    modFit (Event x) stp = retArrow Feed (Event x) (ProcessA stp)
    modFit End stp = retArrow Feed End (ProcessA stp)
    modFit _ stp = stp

    retArrow ph' evx cont = arr $ \(ph, _) -> 
        case ph of
          Suspend -> 
              (ph `mappend` Suspend, if isEnd evx then End else NoEvent, ProcessA $ retArrow ph' evx cont)
          _ -> 
              (ph `mappend` ph', evx, cont)

    pure _ = return $ (End, retArrow Suspend End stopped)

    free (AwaitPF f ff) =
      do
        return $ (NoEvent, arr (uncurry (awaitIt f ff)) >>> proc pc -> pc -<< ())

    free (YieldPF y fc) = return $ (Event y, fit' fc)

    free StopPF = return $ (End, retArrow Suspend End stopped)


    awaitIt f _ Feed (Event x) = proc _ ->
      do
        (evy, stp) <- fit (f x) -< ()
        returnA -< (Feed, evy, ProcessA stp)

    awaitIt _ ff Feed End = proc _ ->
      do
        (evy, stp) <- fit ff -< ()
        returnA -< (Feed, evy, ProcessA stp)

    awaitIt _ ff Sweep End = proc _ ->
      do
        (evy, stp) <- fit ff -< ()
        returnA -< (if not $ isNoEvent evy then Feed else Suspend, evy, ProcessA stp)

    awaitIt f ff ph evx = proc _ ->
        returnA -< (ph `mappend` Suspend, NoEvent, 
                    ProcessA $ arr (uncurry (awaitIt f ff)) >>> proc pc -> pc -<< ())


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
