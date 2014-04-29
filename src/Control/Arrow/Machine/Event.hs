{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module
    Control.Arrow.Machine.Event 
      (
        Occasional (..),
        Event (..), 
        hEv, 
        hEv', 
        evMaybe,
        fromEvent,
        split,
        join,
        split2,
        join2
      )
where


import Control.Monad (liftM, MonadPlus(..))
import Control.Arrow
import Control.Applicative (Applicative(..), Alternative(..), (<$>))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import Data.Monoid (mempty)


class 
    Occasional a
  where
    noEvent :: a
    end :: a
    isNoEvent :: a -> Bool
    isEnd :: a -> Bool
    isOccasion :: a -> Bool
    isOccasion x = not (isNoEvent x) && not (isEnd x)

instance
    (Occasional a, Occasional b) => Occasional (a, b)
  where
    noEvent = (noEvent, noEvent)
    end = (end, end)
    isOccasion xy@(x, y) = 
        (isOccasion x || isOccasion y) && not (isEnd xy)
    isNoEvent xy = 
        not (isOccasion xy) && not (isEnd xy)
    isEnd (x, y) = isEnd x && isEnd y


data Event a = Event a | NoEvent | End deriving (Eq, Show)

instance 
    Occasional (Event a)
  where
    noEvent = NoEvent
    end = End
    isNoEvent NoEvent = True
    isNoEvent _ = False
    isEnd End = True
    isEnd _ = False

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


instance
    Foldable Event
  where
    foldMap f (Event x) = f x
    foldMap _ NoEvent = mempty
    foldMap _ End = mempty


instance
    Traversable Event
  where
    traverse f (Event x) = Event <$> f x
    traverse f NoEvent = pure NoEvent
    traverse f End = pure End


instance
    Alternative Event
  where
    empty = NoEvent
    End <|> _ = End
    _ <|> End = End
    Event x <|> _ = Event x
    NoEvent <|> r = r


instance
    Monad Event
  where
    return = Event

    Event x >>= f = f x
    NoEvent >>= _ = NoEvent
    End >>= _ = End

    _ >> End = End
    l >> r = l >>= const r
    
    fail _ = End


instance
    MonadPlus Event
  where
    mzero = End

    Event x `mplus` _ = Event x
    _ `mplus` Event x = Event x
    End `mplus` r = r
    l `mplus` End = l
    _ `mplus` _ = NoEvent


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
split :: (Arrow a, Occasional b) => a (Event b) b
split = arr go
  where
    go (Event x) = x
    go NoEvent = noEvent
    go End = end


join :: (Arrow a, Occasional b) => a b (Event b)
join = arr go
  where
    go x 
       | isEnd x = End
       | isNoEvent x = NoEvent
       | otherwise = Event x


split2 :: Event (Event a, Event b) -> (Event a, Event b)
split2 = split


join2 :: (Event a, Event b) -> Event (Event a, Event b)
join2 = join
