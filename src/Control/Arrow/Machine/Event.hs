{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module
    Control.Arrow.Machine.Event 
      (
        Event (..), 
        hEv, 
        hEv', 
        evMaybe
      )
where


import qualified Data.Machine as Mc
import Control.Monad (liftM)
import Control.Arrow
import Control.Applicative (Applicative, pure, (<*>))


data Event a = Event a | NoEvent | End

hEv :: ArrowApply a => a (e,b) c -> a e c -> a (e, Event b) c
hEv f1 f2 = proc (e, ev) ->
    helper ev -<< e
  where
    helper (Event x) = proc e -> f1 -< (e, x)
    helper NoEvent = f2
    helper End = f2

hEv' :: ArrowApply a => a (e,b) c -> a e c -> a e c -> a (e, Event b) c
hEv' f1 f2 f3 = proc (e, ev) ->
    helper ev -<< e
  where
    helper (Event x) = proc e -> f1 -< (e, x)
    helper NoEvent = f2
    helper End = f3

instance 
    Functor Event 
  where
    fmap f NoEvent = NoEvent
    fmap f End = End
    fmap f (Event x) = Event (f x)

instance 
    Applicative Event 
  where
    pure = Event
    (Event f) <*> (Event x) = Event $ f x
    End <*> _ = End
    _ <*> End = End
    _ <*> _ = NoEvent

evMaybe :: b -> (a->b) -> Event a -> b
evMaybe _ f (Event x) = f x
evMaybe r _ NoEvent = r
evMaybe r _ End = r
