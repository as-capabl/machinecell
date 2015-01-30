{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module
    Control.Arrow.Machine.Event.Internal
where

import Control.Arrow
import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid (Monoid, mappend, mconcat, mempty)
import Data.Semigroup (Semigroup, (<>))
import Control.Monad (liftM, MonadPlus(..))
    
data Event a = Event a | NoEvent | End deriving (Eq, Show)


instance 
    Functor Event 
  where
    fmap f NoEvent = NoEvent
    fmap f End = End
    fmap f (Event x) = Event (f x)

{-
instance 
    Applicative Event 
  where
    pure = Event

    (Event f) <*> (Event x) = Event $ f x
    End <*> _ = End
    _ <*> End = End
    _ <*> _ = NoEvent
-}

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
    Semigroup a => Monoid (Event a)
  where
    mempty = End
    Event x `mappend` Event y = Event (x <> y)
    Event x `mappend` _ = Event x
    _ `mappend` Event y = Event y
    NoEvent `mappend` _ = NoEvent
    _ `mappend` NoEvent = NoEvent
    _ `mappend` _ = End

{-
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
-}




class 
    Occasional a
  where
    collapse :: a -> Event ()
    noEvent :: a
    end :: a

    isNoEvent :: a -> Bool
    isNoEvent = collapse >>> \case { NoEvent -> True; _ -> False }

    isEnd :: a -> Bool
    isEnd = collapse >>> \case { End -> True; _ -> False }

    isOccasion :: a -> Bool
    isOccasion = collapse >>> \case { Event () -> True; _ -> False }

instance
    (Occasional a, Occasional b) => Occasional (a, b)
  where
    collapse (x, y) = collapse x `mappend` collapse y
    noEvent = (noEvent, noEvent)
    end = (end, end)


instance 
    Occasional (Event a)
  where
    collapse = (() <$)
    noEvent = NoEvent
    end = End

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




evMaybe :: Arrow a => c -> (b->c) -> a (Event b) c
evMaybe r f = arr (go r f)
  where
    go _ f (Event x) = f x
    go r _ NoEvent = r
    go r _ End = r


fromEvent :: Arrow a => b -> a (Event b) b
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


evMap ::  Arrow a => (b->c) -> a (Event b) (Event c)
evMap = arr . fmap


-- TODO: テスト
split :: (Arrow a, Occasional b) => a (Event b) b
split = arr go
  where
    go (Event x) = x
    go NoEvent = noEvent
    go End = end


join :: (Arrow a, Occasional b) => a b (Event b)
join = arr $ \x -> x <$ collapse x


split2 :: Event (Event a, Event b) -> (Event a, Event b)
split2 = split


join2 :: (Event a, Event b) -> Event (Event a, Event b)
join2 = join
