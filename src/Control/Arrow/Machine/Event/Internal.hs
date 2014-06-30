module
    Control.Arrow.Machine.Event.Internal
      (
        Event (..), 
      )
where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid (mappend, mconcat, mempty)

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
