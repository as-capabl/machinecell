
module
    Control.Arrow.Machine.Exception
      (
        catch,
        handle,
        bracket,
        bracket_,
        bracketOnError,
        finally,
        onException
       )
where

import qualified Control.Monad.Trans.Free as F

import Data.Functor ((<$>))

import Control.Arrow.Machine.Types
import Control.Arrow.Machine.Event
import Control.Arrow.Machine.Event.Internal (Event(..))

import Control.Arrow.Machine.Plan.Internal
import Control.Arrow.Machine.Plan

import Debug.Trace

catch :: Monad m =>
    PlanT i o m a -> PlanT i o m a -> PlanT i o m a

catch (F.FreeT mf) cont@(F.FreeT mcont) = 
    F.FreeT $ mf >>= go
  where
    go (F.Pure a) = return $ F.Pure a
    go (F.Free StopPF) = mcont
    go (F.Free (AwaitPF f ff)) = 
        return $ F.Free $ 
        AwaitPF (\i -> f i `catch` cont) (ff `catch` cont)
    go (F.Free fft) = 
        return $ F.Free $ (`catch` cont) <$> fft

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
    before
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
    
