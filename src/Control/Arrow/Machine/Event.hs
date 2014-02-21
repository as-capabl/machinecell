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
        evMaybe,
        fromEvent,
        split2,
        join2
      )
where


import qualified Data.Machine as Mc
import Control.Monad (liftM)
import Control.Arrow
import Control.Applicative (Applicative, pure, (<*>))


data Event a = Event a | NoEvent | End deriving (Eq, Show)

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

fromEvent :: a -> Event a -> a
fromEvent x = evMaybe x id

-- TODO: テスト
condEvent :: Bool -> Event a -> Event a
condEvent _ End = End
condEvent True ev = ev
condEvent False ev = NoEvent

-- TODO: テスト
filterEvent :: (a -> Bool) -> Event a -> Event a
filterEvent cond ev@(Event x) = condEvent (cond x) ev
filterEvent _ ev = ev

-- TODO: テスト
split2 :: Event (Event a, Event b) -> (Event a, Event b)
split2 (Event tp) = tp
split2 NoEvent = (NoEvent, NoEvent)
split2 End = (End, End)

join2 :: (Event a, Event b) -> Event (Event a, Event b)
join2 (End, _) = End
join2 (_, End) = End
join2 (NoEvent, NoEvent) = NoEvent
join2 tp = Event tp
