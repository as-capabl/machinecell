{-# LANGUAGE Safe #-}

module
    Control.Arrow.Machine.Misc.Exception
      (
        -- * Variations of catchP
        -- $variation

        catch,
        handle,
        bracket,
        bracket_,
        bracketOnError,
        finally,
        onException
       )
where

import Control.Arrow.Machine.Types


{-$variation
This module provides variations of catchP.

If you use this module together with "Control.Exception" module of base package,
import this package qualified.
-}

catch :: Monad m =>
    PlanT i o m a -> PlanT i o m a -> PlanT i o m a

catch = catchP


handle :: Monad m =>
    PlanT i o m a -> PlanT i o m a -> PlanT i o m a

handle = flip catch


bracket :: Monad m =>
    PlanT i o m a -> (a -> PlanT i o m b)-> (a -> PlanT i o m c) -> PlanT i o m c
bracket before after thing =
  do
    a <- before
    r <- thing a `catch` (after a >> stop)
    _ <- after a
    return r


bracket_ :: Monad m =>
    PlanT i o m a -> PlanT i o m b-> PlanT i o m c -> PlanT i o m c
bracket_ before after thing =
  do
    _ <- before
    r <- thing `catch` (after >> stop)
    _ <- after
    return r


bracketOnError :: Monad m =>
    PlanT i o m a -> (a -> PlanT i o m b)-> (a -> PlanT i o m c) -> PlanT i o m c
bracketOnError before after thing =
  do
    a <- before
    r <- thing a `catch` (after a >> stop)
    return r


finally :: Monad m =>
    PlanT i o m a -> PlanT i o m b-> PlanT i o m a
finally thing after =
  do
    r <- thing `catch` (after >> stop)
    _ <- after
    return r


onException :: Monad m =>
    PlanT i o m a -> PlanT i o m b-> PlanT i o m a
onException thing after =
  do
    thing `catch` (after >> stop)
    

