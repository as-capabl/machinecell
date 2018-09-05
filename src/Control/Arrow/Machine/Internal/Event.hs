module
    Control.Arrow.Machine.Internal.Event
      (
        -- * Event type and utility
        Occasional' (..),
        Occasional (..),
        Event (..),
        noEvent,
        end,
        ZeroEvent(..),
        condEvent,
        filterEvent,
        filterJust,
        filterLeft,
        filterRight,
        splitEvent,
        evMap
      )
where

import Control.Arrow
import Data.Semigroup (Semigroup ((<>)))
import Data.Void


-- | Discrete events on a time line.
-- Created and consumed by various transducers.
data Event a = Event a | NoEvent | End


instance
    Functor Event
  where
    fmap _ NoEvent = NoEvent
    fmap _ End = End
    fmap f (Event x) = Event (f x)


instance
    Semigroup a => Semigroup (Event a)
  where
    Event x <> Event y = Event (x <> y)
    Event x <> _ = Event x
    _ <> Event y = Event y
    NoEvent <> _ = NoEvent
    _ <> NoEvent = NoEvent
    _ <> _ = End

instance
    Semigroup a => Monoid (Event a)
  where
    mempty = End
    mappend = (<>)

-- | Signals that can be absent(`NoEvent`) or end.
-- For composite structure, `collapse` can be defined as monoid sum of all member occasionals.
class
    Occasional' a
  where
    collapse :: a -> Event ()

-- | Occasional signals with creation methods.
class
    Occasional' a => Occasional a
  where
    burst :: Event Void -> a


instance
    (Occasional' a, Occasional' b) => Occasional' (a, b)
  where
    collapse (x, y) = collapse x `mappend` collapse y

instance
    (Occasional a, Occasional b) => Occasional (a, b)
  where
    burst = burst &&& burst

instance
    Occasional' (Event a)
  where
    collapse = (() <$)

instance
    Occasional (Event a)
  where
    burst = fmap absurd

noEvent :: Occasional a => a
noEvent = burst NoEvent

end :: Occasional a => a
end = burst End

data ZeroEvent = ZeroEvent deriving (Eq, Show, Enum, Bounded)

instance
    Semigroup ZeroEvent
  where
    _ <> _ = ZeroEvent

instance
    Monoid ZeroEvent
  where
    mempty = ZeroEvent
    mappend _ _ = ZeroEvent

instance
    Occasional' ZeroEvent
  where
    collapse _ = mempty


condEvent :: Bool -> Event a -> Event a
condEvent _ End = End
condEvent True ev = ev
condEvent False _ = NoEvent

filterEvent ::
    Arrow ar =>
    (a -> Bool) ->
    ar (Event a) (Event a)
filterEvent cond = filterJust <<< evMap mcond
  where
    mcond x
        | cond x = Just x
        | otherwise = Nothing

filterJust ::
    Arrow ar => ar (Event (Maybe a)) (Event a)
filterJust = arr filterJust'
  where
    filterJust' (Event (Just x)) = Event x
    filterJust' (Event Nothing) = NoEvent
    filterJust' NoEvent = NoEvent
    filterJust' End = End

-- |Split an event stream.
--
-- >> run (filterLeft) [Left 1, Right 2, Left 3, Right 4]
-- [1,3]
filterLeft ::
    Arrow ar =>
    ar (Event (Either a b)) (Event a)
filterLeft = filterJust <<< evMap (either Just (const Nothing))

-- |Split an event stream.
--
-- >> run filterRight [Left 1, Right 2, Left 3, Right 4]
-- [2,4]
filterRight ::
    Arrow ar =>
    ar (Event (Either a b)) (Event b)
filterRight = filterJust <<< evMap (either (const Nothing) Just)

-- |Split an event stream.
--
-- >> run (splitEvent >>> arr fst) [Left 1, Right 2, Left 3, Right 4]
-- [1,3]
--
-- >> run (splitEvent >>> arr snd) [Left 1, Right 2, Left 3, Right 4]
-- [2,4]
splitEvent ::
    Arrow ar =>
    ar (Event (Either a b)) (Event a, Event b)
splitEvent = filterLeft &&& filterRight

-- | Alias of "arr . fmap"
--
-- While "ProcessT a (Event b) (Event c)" means a transducer from b to c,
-- function b->c can be lifted into a transducer by fhis function.
--
-- But in most cases you needn't call this function in proc-do notations,
-- because `arr`s are completed automatically while desugaring.
--
-- For example,
--
-- @
-- proc x -> returnA -\< f \<$\> x
-- @
--
-- is equivalent to
--
-- @
-- evMap f
-- @
evMap ::  Arrow a => (b->c) -> a (Event b) (Event c)
evMap = arr . fmap

