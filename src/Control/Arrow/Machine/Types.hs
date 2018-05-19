{-# LANGUAGE Trustworthy #-} -- Safe if eliminate GeneralizedNewtypeInstance
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module
    Control.Arrow.Machine.Types
      (
        -- * Stream transducer type
        ProcessT(),
        ProcessA,

        -- * Event type and utility
        Occasional' (..),
        Occasional (..),
        Event (),
        noEvent,
        end,
        ZeroEvent(..),
        condEvent,
        filterEvent,
        filterJust,
        filterLeft,
        filterRight,
        splitEvent,
        evMap,

        -- * Coroutine monad
        -- | Procedural coroutine monad that can await or yield values.
        --
        -- Coroutines can be encoded to machines by `constructT` or so on and
        -- then put into `ProcessT` compositions.
        PlanT(..),
        Plan,

        MonadAwait (..),
        MonadYield (..),
        MonadStop (..),
        catchP,

        stopped,
        muted,

        -- * Constructing machines from plans
        constructT,
        repeatedlyT,

        construct,
        repeatedly,

        -- * Evolution monad
        -- | Time-evolution monad, or generalized plan monad.
        Evolution(..),
        packProc,
        awaitProc,
        yieldProc,

        -- * Running machines (at once)
        runT,
        runT_,
        run,
        run_,

        -- * Running machines (step-by-step)
        stepRun,
        stepYield,

        -- * Primitive machines - switches
        -- | Switches inspired by the Yampa library.
        -- Signature is almost same, but collection requirement is  not only 'Functor',
        -- but 'Tv.Traversable'. This is because of side effects.
        switch,
        dSwitch,
        rSwitch,
        drSwitch,
        kSwitch,
        dkSwitch,
        gSwitch,
        dgSwitch,
        pSwitch,
        pSwitchB,
        dpSwitch,
        dpSwitchB,
        rpSwitch,
        rpSwitchB,
        drpSwitch,
        drpSwitchB,
        par,
        parB,

        -- * Primitive machines - other safe primitives
        fit,
        fitW,

        -- * Primitive machines - unsafe
        unsafeExhaust,
      )
where

import qualified Control.Category as Cat
import Data.Profunctor (Profunctor, dimap, rmap)
import Data.Void
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer hiding ((<>))
import Control.Monad.Identity
import Control.Monad.Trans.Cont
import Control.Applicative
import qualified Data.Foldable as Fd
import Data.Traversable as Tv
import Data.Semigroup (Semigroup ((<>)))
import Data.Maybe (fromMaybe, isNothing, isJust)
import qualified Control.Monad.Trans.Free.Church as F
import GHC.Exts (build)


-- | To get multiple outputs by one input, the `Phase` parameter is introduced.
--
-- Once a value `Feed`ed, the machine is `Sweep`ed until it `Suspend`s.
data Phase = Feed | Sweep | Suspend deriving (Eq, Show)

instance
    Semigroup Phase
  where
    (<>) Feed _ = Feed
    (<>) _ Feed = Feed
    (<>) Suspend _ = Suspend
    (<>) _ Suspend = Suspend
    (<>) Sweep Sweep = Sweep

instance
    Monoid Phase
  where
    mempty = Sweep

    mappend = (<>)


type ProcType a b c = ProcessT a b c

class Stepper m b c s | s -> m, s -> b, s -> c
  where
    feed :: s -> b -> m (c, s)
    sweep :: s -> b -> m (Maybe c, s)
    suspend :: s -> b -> c

-- | The stream transducer arrow.
--
-- To construct `ProcessT` instances, use `Control.Arrow.Machine.Plan.Plan`,
-- `arr`, functions declared in `Control.Arrow.Machine.Utils`,
-- or arrow combinations of them.
--
-- See an introduction at "Control.Arrow.Machine" documentation.
{-
data ProcessT m b c = ProcessT {
    paFeed :: b -> m (c, ProcessT m b c),
    paSweep :: b -> m (Maybe c, ProcessT m b c),
    paSuspend :: !(b -> c)
  }
-}

-- | Isomorphic to ProcessT when 'a' is ArrowApply.
type ProcessA a = ProcessT (ArrowMonad a)

instance
    Stepper a b c (ProcessT a b c)
  where
    feed (ProcessT evoF) x = case x
      of
        Aw y0 f _ -> weakFeed y0 (f x) x
        Yd _ y p -> return (y, p)
        M mk = mk >>= \p -> feed p x
    sweep = undefined
    suspend = undefined

toProcessT ::
    (Monad m, Stepper m b c s) =>
    s -> ProcessT m b c
toProcessT s = ProcessT {
    paFeed = liftM (second toProcessT) . feed s,
    paSweep = liftM (second toProcessT) . sweep s,
    paSuspend = suspend s
  }
{-# INLINE[2] toProcessT  #-}

-- For internal use
class
    (Applicative f, Monad f) => ProcessHelper f
  where
    step ::
        (Monad m, Stepper m b c s) =>
        s -> b -> m (f c, s)
    helperToMaybe :: f a -> Maybe a
    weakly :: a -> f a

    compositeStep ::
        (Monad m, Stepper m b p s1, Stepper m p c s2) =>
        s1 -> s2 ->
        b -> m (f c, s1, s2)


instance
    ProcessHelper Identity
  where
    step pa = liftM (first Identity) . feed pa
    helperToMaybe = Just . runIdentity
    weakly = Identity
    compositeStep sf test x =
      do
        (y, sf') <- feed sf x
        (z, test') <- feed test y
        return (return z, sf', test')

instance
    ProcessHelper Maybe
  where
    step = sweep
    helperToMaybe = id
    weakly _ = Nothing
    compositeStep sf0 test0 x =
      do
        let y = suspend sf0 x
        (mt, test') <- sweep test0 y
        case mt
          of
            Just t -> return (Just t, sf0, test')
            Nothing -> next sf0 test'

      where
        next sf test =
          do
            (my, sf') <- sweep sf x
            case my
              of
                Just y -> next2 y sf' test
                Nothing -> return (Nothing, sf', test)

        next2 y sf test =
          do
            (t, test') <- feed test y
            return (Just t, sf, test')

makePA ::
    Monad m =>
    (forall f. ProcessHelper f =>
        b -> m (f c, ProcessT m b c)) ->
    (b -> c) ->
    ProcessT m b c
makePA h !sus = ProcessT {
    paFeed = liftM (first runIdentity) . h,
    paSweep = h,
    paSuspend = sus
  }


data CompositeStep m b c s1 s2
  where
    CompositeStep ::
        (Stepper m b p s1, Stepper m p c s2) =>
        s1 -> s2 ->
        CompositeStep m b c s1 s2

instance
    Monad m => Stepper m b c (CompositeStep m b c s1 s2)
  where
    feed (CompositeStep s1 s2) x =
      do
        (fz, s1', s2') <- compositeStep s1 s2 x
        return (runIdentity fz, CompositeStep s1' s2')
    sweep (CompositeStep s1 s2) x =
      do
        (fz, s1', s2') <- compositeStep s1 s2 x
        return (fz, CompositeStep s1' s2')
    suspend (CompositeStep s1 s2) =
        suspend s2 . suspend s1


data IDStep m b c
  where
    IDStep :: IDStep (m :: * -> *) b b

instance
    Monad m => Stepper m b c (IDStep m b c)
  where
    feed IDStep x = return (x, IDStep)
    sweep IDStep _ = return (Nothing, IDStep)
    suspend IDStep = id

newtype ArrStep (m :: * -> *) b c = ArrStep (b -> c)

instance
    Monad m => Stepper m b c (ArrStep m b c)
  where
    feed (ArrStep f) x = return (f x, ArrStep f)
    sweep (ArrStep f) _ = return (Nothing, ArrStep f)
    suspend (ArrStep f) = f


data ParStep m b c s1 s2
  where
    ParStep ::
        (Stepper m b1 c1 s1, Stepper m b2 c2 s2) =>
        s1 -> s2 ->
        ParStep m (b1, b2) (c1, c2) s1 s2

instance
    Monad m => Stepper m b c (ParStep m b c s1 s2)
  where
    feed (ParStep f g)  (x1, x2) =
      do
        (y1, f') <- feed f x1
        (y2, g') <- feed g x2
        return ((y1, y2), ParStep f' g')
    sweep (ParStep f g) (x1, x2) =
      do
        (my1, f') <- sweep f x1
        (my2, g') <- sweep g x2
        let y1 = fromMaybe (suspend f' x1) my1 -- suspend f ?
            y2 = fromMaybe (suspend g' x2) my2
            r = if (isNothing my1 && isNothing my2) then Nothing else Just (y1, y2)
        return (r, ParStep f' g')
    suspend (ParStep f g) = suspend f *** suspend g


-- |Natural transformation
fit ::
    (Monad m, Monad m') =>
    (forall p. m p -> m' p) ->
    ProcessT m b c -> ProcessT m' b c
fit f pa =
    arr Identity >>>
    fitW runIdentity (\ar (Identity x) -> f (ar x)) pa

-- |Experimental: more general fit.
--
-- Should w be a comonad?
fitW :: (Monad m, Monad m', Functor w) =>
    (forall p. w p -> p) ->
    (forall p q. (p -> m q) -> w p -> m' q) -> 
    ProcessT m b c -> ProcessT m' (w b) c
fitW extr f pa = makePA
    (liftM (second $ fitW extr f) . f (step pa))
    (extr >>> suspend pa)

instance
    Monad m => Profunctor (ProcessT m)
  where
    dimap = dimapProc
    {-# INLINE dimap #-}

dimapProc ::
    Monad m =>
    (b->c)->(d->e)->
    ProcType m c d -> ProcType m b e
dimapProc f g pa = makePA
    (liftM (fmap g *** dimapProc f g) . step pa . f)
    (dimap f g (suspend pa))

{-# NOINLINE dimapProc #-}


instance
    Monad m => Functor (ProcessT m i)
  where
    fmap = rmap

instance
    Monad m => Applicative (ProcessT m i)
  where
    pure = arr . const
    pf <*> px = (pf &&& px) >>> arr (uncurry ($))

instance
    (Monad m, Semigroup o) => Semigroup (ProcessT m i o)
  where
    (<>) = liftA2 (<>)

instance
    (Monad m, Monoid o) => Monoid (ProcessT m i o)
  where
    mempty = pure mempty
    mappend = liftA2 mappend

instance
    Monad m => Cat.Category (ProcessT m)
  where
    id = idProc
    {-# INLINE id #-}

    g . f = compositeProc f g
    {-# INLINE (.) #-}


instance
    Monad m => Arrow (ProcessT m)
  where
    arr = arrProc
    {-# INLINE arr #-}

    first pa = parProc pa idProc
    {-# INLINE first #-}

    second pa = parProc idProc pa
    {-# INLINE second #-}

    (***) = parProc
    {-# INLINE (***) #-}


parProc :: Monad m =>
    ProcType m b c ->
    ProcType m d e ->
    ProcType m (b, d) (c, e)
parProc f g = toProcessT $ ParStep f g
{-# INLINE [0] parProc #-}

idProc :: Monad m => ProcType m b b
idProc = let pa = makePA (\x -> return (weakly x, pa)) id in pa
{-# NOINLINE idProc #-}

arrProc :: Monad m => (b->c) -> ProcType m b c
arrProc f = let pa = makePA (\x -> return (weakly (f x), pa)) f in pa
{-# NOINLINE arrProc #-}

-- |Composition is proceeded by the backtracking strategy.
compositeProc :: Monad m =>
              ProcType m b d -> ProcType m d c -> ProcType m b c
compositeProc f0 g0 = ProcessT {
    paFeed = \x ->
      do
        (y, f') <- feed f0 x
        (z, g') <- feed g0 y
        return (z, compositeProc f' g'),
    paSweep = \x ->
      do
        (mz, g') <- sweep g0 $ suspend f0 x
        case mz
          of
            Just z -> return (Just z, compositeProc f0 g')
            Nothing -> btrk f0 g' x,
    paSuspend = suspend f0 >>> suspend g0
  }
  where
    btrk f g x =
      do
        (my, f') <- sweep f x
        (mz, g') <-
            case my
              of
                Just y ->
                  do
                    (z, g') <- feed g y
                    return (Just z, g')
                Nothing ->
                    return (Nothing, g)
        return (mz, compositeProc f' g')

{-# NOINLINE compositeProc #-}

-- rules
{-# RULES
"ProcessT: id/*"
    forall g. compositeProc idProc g = g
"ProcessT: */id"
    forall f. compositeProc f idProc = f

"ProcessT: concat/concat"
    forall f g h. compositeProc (compositeProc f g) h = compositeProc f (compositeProc g h)

"ProcessT: dimap/dimap"
    forall f g h i j. dimapProc f j (dimapProc g i h)  = dimapProc (g . f) (j . i) h
"ProcessT: dimap/arr"
    forall f g h. dimapProc f h (arrProc g) = arrProc (h . g . f)

"ProcessT: arr***/par"
    forall f1 f2 g1 g2 h. compositeProc (parProc f1 (arrProc f2)) (compositeProc (parProc g1 g2) h) =
        compositeProc (parProc (compositeProc f1 g1) (dimapProc f2 id g2)) h
"ProcessT: arr***/par-2"
    forall f1 f2 g1 g2. compositeProc (parProc f1 (arrProc f2)) (parProc g1 g2) =
        parProc (compositeProc f1 g1) (dimapProc f2 id g2)
"ProcessT: par/***arr"
    forall f1 f2 g1 g2 h. compositeProc (parProc f1 f2) (compositeProc (parProc (arrProc g1) g2) h) =
        compositeProc (parProc (dimapProc id g1 f1) (compositeProc f2 g2)) h
"ProcessT: par/***arr-2"
    forall f1 f2 g1 g2. compositeProc (parProc f1 f2) (parProc (arrProc g1) g2) =
        parProc (dimapProc id g1 f1) (compositeProc f2 g2)

"ProcessT: first/par"
    forall f1 g1 g2 h. compositeProc (parProc f1 idProc) (compositeProc (parProc g1 g2) h) =
        compositeProc (parProc (compositeProc f1 g1) g2) h
"ProcessT: first/par-2"
    forall f1 g1 g2. compositeProc (parProc f1 idProc) (parProc g1 g2) =
        parProc (compositeProc f1 g1) g2
"ProcessT: par/second"
    forall f1 f2 g2 h. compositeProc (parProc f1 f2) (compositeProc (parProc idProc g2) h) =
        compositeProc (parProc f1 (compositeProc f2 g2)) h
"ProcessT: par/second-2"
    forall f1 f2 g2. compositeProc (parProc f1 f2) (parProc idProc g2) =
        parProc f1 (compositeProc f2 g2)

"ProcessT: arr/arr"
    forall f g h. compositeProc (arrProc f) (compositeProc (arrProc g) h) =
        compositeProc (arrProc (g . f)) h
"ProcessT: arr/arr-2"
    forall f g. compositeProc (arrProc f) (arrProc g) = arrProc (g . f)
"ProcessT: arr/*" [1]
    forall f g. compositeProc (arrProc f) g = dimapProc f id g
"ProcessT: */arr" [1]
    forall f g. compositeProc f (arrProc g) = dimapProc id g f
"ProcessT: arr***arr" [1]
    forall f g. parProc (arrProc f) (arrProc g) = arrProc (f *** g)
  #-}

instance
    Monad m => ArrowChoice (ProcessT m)
  where
    left pa0 = makePA
        (\eth -> sweep' pa0 eth)
        (left $ suspend pa0)
      where
        sweep' pa (Left x) =
          do
            (my, pa') <- step pa x
            return (Left <$> my, left pa')
        sweep' pa (Right d) =
            return (weakly (Right d), left pa)

instance
    Monad m => ArrowLoop (ProcessT m)
  where
    loop pa =
        makePA
            (\x ->
              do
                (hyd, pa') <- step pa (x, loopSusD x)
                return (fst <$> hyd, loop pa'))
            (loop $ suspend pa)
      where
        loopSusD = loop (suspend pa >>> \(_, d) -> (d, d))


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
-- >>> run (filterLeft) [Left 1, Right 2, Left 3, Right 4]
-- [1,3]
filterLeft ::
    Arrow ar =>
    ar (Event (Either a b)) (Event a)
filterLeft = filterJust <<< evMap (either Just (const Nothing))

-- |Split an event stream.
--
-- >>> run filterRight [Left 1, Right 2, Left 3, Right 4]
-- [2,4]
filterRight ::
    Arrow ar =>
    ar (Event (Either a b)) (Event b)
filterRight = filterJust <<< evMap (either (const Nothing) Just)

-- |Split an event stream.
--
-- >>> run (splitEvent >>> arr fst) [Left 1, Right 2, Left 3, Right 4]
-- [1,3]
--
-- >>> run (splitEvent >>> arr snd) [Left 1, Right 2, Left 3, Right 4]
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



muted ::
    (Monad m, Occasional' b, Occasional c) => ProcessT m b c
muted = arr collapse >>> repeatedly await >>> arr burst

data EvoF_ i o m r
  where
    Aw :: o -> (i -> r) -> r -> EvoF i o m r
    Yd :: o -> o -> r -> EvoF i o m r
    M :: o -> m r -> EvoF i o m r

newtype EvoF i o m r = EvoF (i -> EvoF_ i o m r) deriving Functor

instance
    Functor m => Functor (EvoF_ i o m r)
  where
    fmap g (Aw y f ff) = Aw y (g . f) (g ff)

newtype Evolution i o m r = Evolution (forall b. (r -> b) -> (EvoF i o m b -> b) -> b -> b)

newtype ProcessT m i o = ProcessT (EvoF i o m (ProcessT m i o))

instance
    Functor (Evolution i o m r)
  where
    fmap g (Evolution k) = Evolution (\pr fr z -> k (pr . g) fr z)



instance
    Occasional o =>
    MonadTrans (Evolution i o)
  where
    {-# INLINE lift #-}
    lift ma = Evolution $ cont $ \fmpf -> packProc (fmpf <$> ma)

instance
    (MonadIO m, Occasional o) =>
    MonadIO (Evolution i o m)
  where
    {-# INLINE liftIO #-}
    liftIO ma = lift $ liftIO ma


data
    PlanF i o a
  where
    AwaitPF :: (i->a) -> a -> PlanF i o a
    YieldPF :: o -> a -> PlanF i o a
    StopPF :: PlanF i o a

instance
    Functor (PlanF i o)
  where
    fmap g (AwaitPF f ff) = AwaitPF (g . f) (g ff)
    fmap g (YieldPF x r) = YieldPF x (g r)
    fmap _ StopPF = StopPF


newtype PlanT i o m a =
    PlanT { freePlanT :: F.FT (PlanF i o) m a }
  deriving
    (Functor, Applicative, Monad)

type Plan i o a = forall m. Monad m => PlanT i o m a

packProc ::
    (Monad m, Occasional o) =>
    m (ProcessT m i o) ->
    ProcessT m i o
packProc !mp = ProcessT {
    paFeed = \ex -> mp >>= \p -> feed p ex ,
    paSweep = \ex -> mp >>= \p -> sweep p ex,
    paSuspend = const noEvent
  }
{-# INLINE[0] packProc #-}
{-# RULES
"ProcessT: return/packProc"
    forall p. return (packProc p) = p
 #-}
{-
"ProcessT: packProc/return"
    forall p. packProc (return p) = p
 -}

instance
    MonadTrans (PlanT i o)
  where
    lift mx = PlanT $ lift mx
    {-# INLINE lift #-}

instance
    MonadReader r m => MonadReader r (PlanT i o m)
  where
    ask = lift ask
    local f mx = PlanT $ local f (freePlanT mx)

instance
    MonadWriter w m => MonadWriter w (PlanT i o m)
  where
    tell = lift . tell
    listen mx = PlanT $ listen (freePlanT mx)
    pass mx = PlanT $ pass (freePlanT mx)

instance
    MonadState s m => MonadState s (PlanT i o m)
  where
    get = lift get
    put x = lift $ put x

instance
    Monad m => Alternative (PlanT i o m)
  where
    empty = stop
    (<|>) = catchP

instance
    Monad m => MonadPlus (PlanT i o m)
  where
    mzero = stop
    mplus = catchP

instance
    MonadIO m => MonadIO (PlanT i o m)
  where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

class
    MonadAwait m a | m -> a
  where
    await :: m a

instance
    Monad m => MonadAwait (PlanT i o m) i
  where
    {-# INLINE await #-}
    await = PlanT $ F.wrap $ AwaitPF return (F.liftF StopPF)

instance
    (Monad m, Occasional o) =>
    MonadAwait (Evolution (Event a) o m) a
  where
    {-# INLINE await #-}
    await = Evolution $ cont $ \next -> awaitProc next stopped

class
    MonadYield m a | m -> a
  where
    yield :: a -> m ()

instance
    Monad m => MonadYield (PlanT i o m) o
  where
    {-# INLINE yield #-}
    yield x = PlanT $ F.liftF $ YieldPF x ()

instance
    Monad m => MonadYield (Evolution i (Event a) m) a
  where
    {-# INLINE yield #-}
    yield x = Evolution $ cont $ \next -> yieldProc x (next ())

class
    MonadStop m
  where
    stop :: m a

instance
    Monad m => MonadStop (PlanT i o m)
  where
    {-# INLINE stop #-}
    stop = PlanT $ F.liftF StopPF

instance
    (Monad m, Occasional o) =>
    MonadStop (Evolution i o m)
  where
    {-# INLINE stop #-}
    stop = Evolution $ cont $ const stopped

catchP:: Monad m =>
    PlanT i o m a -> PlanT i o m a -> PlanT i o m a

catchP (PlanT pl) next0 =
    PlanT $ F.FT $ \pr free ->
        F.runFT pl pr (free' next0 pr free)
  where
    free' ::
        Monad m =>
        PlanT i o m a ->
        (a -> m r) ->
        (forall x. (x -> m r) -> PlanF i o x -> m r) ->
        (y -> m r) ->
        (PlanF i o y) ->
        m r
    free' (PlanT next) pr free r pl' =
        let nextR = F.runFT next pr free
            go StopPF = nextR
            go (AwaitPF f ff) =
                free (either (\_ -> nextR) r) $ AwaitPF (Right . f) (Left ff)
            go _ = free r pl'
          in
            go pl'

{-# INLINE awaitProc #-}
awaitProc ::
    (Monad m, Occasional o) =>
    (a -> ProcessT m (Event a) o) ->
    ProcessT m (Event a) o ->
    ProcessT m (Event a) o
awaitProc f ff = awaitProc'
  where
    awaitProc' = ProcessT {
        paFeed = awaitFeed,
        paSweep = awaitSweep,
        paSuspend = const noEvent
      }

    awaitFeed (Event x) = feed (f x) NoEvent
    awaitFeed NoEvent = return (noEvent, awaitProc')
    awaitFeed End = feed ff End

    awaitSweep (Event x) = sweep (f x) NoEvent
    awaitSweep NoEvent = return (Nothing, awaitProc')
    awaitSweep End = sweep ff End

{-# INLINE yieldProc #-}
yieldProc ::
    Monad m =>
    a ->
    ProcessT m i (Event a) ->
    ProcessT m i (Event a)
yieldProc y pa = ProcessT {
    paFeed = \_ -> return (Event y, pa),
    paSweep = \_ -> return (Just (Event y), pa),
    paSuspend = const NoEvent
  }

{-# INLINE stopped #-}
stopped ::
    (Monad m, Occasional o) =>
    ProcessT m i o
stopped = ProcessT {
    paFeed = \_ -> return (end, arr (const end)),
    paSweep = \_ -> return (Just end, arr (const end)),
    paSuspend = pure end
  }

{-# INLINE constructT #-}
constructT ::
    (Monad m) =>
    PlanT i o m r ->
    ProcessT m (Event i) (Event o)
constructT pl0 = runCont (runEvolution $ realizePlan pl0) (const stopped)

{-# INLINE realizePlan #-}
realizePlan ::
    Monad m =>
    PlanT i o m a ->
    Evolution (Event i) (Event o) m a
realizePlan pl = Evolution $ cont $ \next ->
    packProc $ F.runFT (freePlanT pl) (return . next) (\b fr -> return $ free (packProc . b <$> fr))
  where
    free ::
        Monad m => PlanF i o (ProcessT m (Event i) (Event o)) -> ProcessT m (Event i) (Event o)
    free (AwaitPF f ff) = awaitProc f ff
    free (YieldPF y pa) = yieldProc y pa
    free StopPF = stopped

{-# INLINE repeatedlyT #-}
repeatedlyT ::
    Monad m =>
    PlanT i o m r ->
    ProcessT m (Event i) (Event o)
repeatedlyT pl0 = runCont (forever $ runEvolution $ realizePlan pl0) absurd


-- for pure
{-# INLINE construct #-}
construct ::
    Monad m =>
    PlanT i o Identity r ->
    ProcessT m (Event i) (Event o)
construct = fit (return . runIdentity) . constructT

{-# INLINE repeatedly #-}
repeatedly ::
    Monad m =>
    PlanT i o Identity r ->
    ProcessT m (Event i) (Event o)
repeatedly = fit (return . runIdentity) . repeatedlyT


--
-- Switches
--

-- |Run the 1st transducer at the beggining. Then switch to 2nd when Event t occurs.
--
-- >>> :{
-- let
--     before = proc x ->
--       do
--         trigger <- filterEvent (== 3) -< x
--         returnA -< ((*10) <$> x, trigger)
--     after t = proc x -> returnA -< (*100) <$> x
--  in
--     run (switch before after) [1..5]
-- :}
-- [10,20,300,400,500]
switch ::
    Monad m =>
    ProcessT m b (c, Event t) ->
    (t -> ProcessT m b c) ->
    ProcessT m b c
switch sf k = ggSwitch (const ()) sf (\() -> k)


-- |Delayed version of `switch`
--
-- >>> :{
-- let
--     before = proc x ->
--       do
--         trigger <- filterEvent (== 3) -< x
--         returnA -< ((*10) <$> x, trigger)
--     after t = proc x -> returnA -< (*100) <$> x
--  in
--     run (dSwitch before after) [1..5]
-- :}
-- [10,20,30,400,500]
dSwitch ::
    Monad m =>
    ProcessT m b (c, Event t) ->
    (t -> ProcessT m b c) ->
    ProcessT m b c
dSwitch sf k = dggSwitch (const ()) sf (\() -> k)

-- |Recurring switch.
--
-- >>> :{
-- let pa = proc evtp ->
--       do
--         evx <- returnA -< fst <$> evtp
--         evarr <- filterJust -< snd <$> evtp
--         rSwitch (evMap (*10)) -< (evx, evarr)
--     l = [(1, Nothing),
--          (2, Just (arr $ fmap (*100))),
--          (3, Nothing),
--          (4, Just (arr $ fmap (*1000))),
--          (5, Nothing)]
--   in
--     run pa l
-- :}
-- [10,200,300,4000,5000]
rSwitch ::
    Monad m =>
    ProcessT m b c ->
    ProcessT m (b, Event (ProcessT m b c)) c
rSwitch p = rSwitch' (p *** Cat.id) >>> arr fst
  where
    rSwitch' pid = kSwitch pid test $ \_ p' -> rSwitch'' (p' *** Cat.id)
    rSwitch'' pid = dkSwitch pid test $ \s _ -> rSwitch' s
    test = proc (_, (_, r)) -> returnA -< r


-- |Delayed version of `rSwitch`.
--
-- >>> :{
-- let pa = proc evtp ->
--       do
--         evx <- returnA -< fst <$> evtp
--         evarr <- filterJust -< snd <$> evtp
--         drSwitch (evMap (*10)) -< (evx, evarr)
--     l = [(1, Nothing),
--          (2, Just (arr $ fmap (*100))),
--          (3, Nothing),
--          (4, Just (arr $ fmap (*1000))),
--          (5, Nothing)]
--   in
--     run pa l
-- :}
-- [10,20,300,400,5000]
drSwitch ::
    Monad m => ProcessT m b c ->
    ProcessT m (b, Event (ProcessT m b c)) c

drSwitch p =  drSwitch' (p *** Cat.id)
  where
    drSwitch' pid = dSwitch pid $ \p' -> drSwitch' (p' *** Cat.id)


kSwitch ::
    Monad m =>
    ProcessT m b c ->
    ProcessT m (b, c) (Event t) ->
    (ProcessT m b c -> t -> ProcessT m b c) ->
    ProcessT m b c
kSwitch sf test =
    ggSwitch
        (\(CompositeStep _ (CompositeStep (ParStep IDStep sf') _)) -> sf')
        (CompositeStep (ArrStep (id &&& id))
           (CompositeStep (ParStep IDStep sf) (arr snd &&& test)))


dkSwitch ::
    Monad m =>
    ProcessT m b c ->
    ProcessT m (b, c) (Event t) ->
    (ProcessT m b c -> t -> ProcessT m b c) ->
    ProcessT m b c
dkSwitch sf test =
    dggSwitch
        (\(CompositeStep _ (CompositeStep (ParStep IDStep sf') _)) -> sf')
        (CompositeStep (ArrStep (id &&& id))
           (CompositeStep (ParStep IDStep sf) (arr snd &&& test)))

ggSwitch ::
    (Monad m, Stepper m b (c, Event t) sWhole) =>
    (sWhole -> s) ->
    sWhole ->
    (s -> t -> ProcessT m b c) ->
    ProcessT m b c
ggSwitch picker whole k = makePA
    (\x ->
      do
        let
        (hyevt, whole') <- step whole x
        let hy = fst <$> hyevt
            hevt = snd <$> hyevt
        case (helperToMaybe hevt)
          of
            Just (Event t) -> step (k (picker whole') t) x
            _ -> return (hy, ggSwitch picker whole' k))
    (arr fst . suspend whole)

dggSwitch ::
    (Monad m, Stepper m b (c, Event t) sWhole) =>
    (sWhole -> s) ->
    sWhole ->
    (s -> t -> ProcessT m b c) ->
    ProcessT m b c
dggSwitch picker whole k = makePA
    (\x ->
      do
        let
        (hyevt, whole') <- step whole x
        let hy = fst <$> hyevt
            hevt = snd <$> hyevt
        case (helperToMaybe hevt)
          of
            Just (Event t) -> return (hy, k (picker whole') t)
            _ -> return (hy, dggSwitch picker whole' k))
    (arr fst . suspend whole)

gSwitch ::
    Monad m =>
    ProcessT m b (p, r) ->
    ProcessT m p q ->
    ProcessT m (q, r) (c, Event t) ->
    (ProcessT m p q -> t -> ProcessT m b c) ->
    ProcessT m b c
gSwitch pre sf post =
    ggSwitch
        (\(CompositeStep _ (CompositeStep (ParStep sf' IDStep) _)) -> sf')
        (CompositeStep pre (CompositeStep (ParStep sf IDStep) post))

dgSwitch ::
    Monad m =>
    ProcessT m b (p, r) ->
    ProcessT m p q ->
    ProcessT m (q, r) (c, Event t) ->
    (ProcessT m p q -> t -> ProcessT m b c) ->
    ProcessT m b c
dgSwitch pre sf post =
    dggSwitch
        (\(CompositeStep _ (CompositeStep (ParStep sf' IDStep) _)) -> sf')
        (CompositeStep pre (CompositeStep (ParStep sf IDStep) post))

broadcast ::
    Functor col =>
    b -> col sf -> col (b, sf)
broadcast x sfs = fmap (\sf -> (x, sf)) sfs

par ::
    (Monad m, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessT m ext c) ->
    ProcessT m b (col c)
par r sfs = toProcessT (PluralStep r sfs)

parB ::
    (Monad m, Tv.Traversable col) =>
    col (ProcessT m b c) ->
    ProcessT m b (col c)
parB = par broadcast


data PluralStep ext col m b c
  where
    PluralStep ::
        (forall sf. (b -> col sf -> col (ext, sf))) ->
        (col (ProcessT m ext c)) ->
        PluralStep ext col m b c


instance
    (Monad m, Tv.Traversable col) =>
    Stepper m b (col c) (PluralStep ext col m b c)
  where
    feed (PluralStep r sfs) = liftM (runIdentity *** PluralStep r) . parCore r sfs
    sweep (PluralStep r sfs) = liftM (id *** PluralStep r) . parCore r sfs
    suspend (PluralStep r sfs) = suspendAll r sfs

suspendAll ::
    (Monad m, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessT m ext c) ->
    b -> col c
suspendAll r sfs = (sus <$>) . (r `flip` sfs)
  where
    sus (ext, sf) = suspend sf ext

traverseResult ::
    forall h col c.
    (Tv.Traversable col, ProcessHelper h) =>
    col (h c, c) -> h (col c)
traverseResult zs =
    let
        pr :: (h c, c) -> StateT Bool h c
        pr (hx, d) =
          do
            let mx = helperToMaybe hx
            if isJust mx then put True else return ()
            return (fromMaybe d mx)
        hxs = runStateT (Tv.sequence (pr <$> zs)) False
        exist = fromMaybe False $ helperToMaybe (snd <$> hxs)
        result = fst <$> hxs
      in
        if exist then result else join (weakly result)

parCore ::
    (Applicative m, Monad m, Tv.Traversable col, ProcessHelper h) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessT m ext c) ->
    b -> m (h (col c), col (ProcessT m ext c))
parCore r sfs x =
  do
    let input = r x sfs
    ret <- Tv.sequenceA $ fmap app' input
    let zs = traverseResult $ fmap fst ret
        sfs' = fmap snd ret
    return (zs, sfs')
  where
    app' (y, sf) =
      do
        (hz, sf') <- step sf y
        return ((hz, suspend sf' y), sf')

pSwitch ::
    (Monad m, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessT m ext c) ->
    ProcessT m (b, col c) (Event mng) ->
    (col (ProcessT m ext c) -> mng -> ProcessT m b (col c)) ->
    ProcessT m b (col c)
pSwitch r sfs test =
    ggSwitch
        (\(CompositeStep _
            (CompositeStep (ParStep IDStep (PluralStep _ sfs')) _)) -> sfs')
        (CompositeStep (ArrStep (id &&& id))
            (CompositeStep (ParStep IDStep (PluralStep r sfs)) (arr snd &&& test)))

pSwitchB ::
    (Monad m, Tv.Traversable col) =>
    col (ProcessT m b c) ->
    ProcessT m (b, col c) (Event mng) ->
    (col (ProcessT m b c) -> mng -> ProcessT m b (col c)) ->
    ProcessT m b (col c)
pSwitchB = pSwitch broadcast

dpSwitch ::
    (Monad m, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessT m ext c) ->
    ProcessT m (b, col c) (Event mng) ->
    (col (ProcessT m ext c) -> mng -> ProcessT m b (col c)) ->
    ProcessT m b (col c)
dpSwitch r sfs test =
    dggSwitch
        (\(CompositeStep _
            (CompositeStep (ParStep IDStep (PluralStep _ sfs')) _)) -> sfs')
        (CompositeStep (ArrStep (id &&& id))
            (CompositeStep (ParStep IDStep (PluralStep r sfs)) (arr snd &&& test)))

dpSwitchB ::
    (Monad m, Tv.Traversable col) =>
    col (ProcessT m b c) ->
    ProcessT m (b, col c) (Event mng) ->
    (col (ProcessT m b c) -> mng -> ProcessT m b (col c)) ->
    ProcessT m b (col c)
dpSwitchB = dpSwitch broadcast

rpSwitch ::
    (Monad m, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessT m ext c) ->
    ProcessT m
        (b, Event (col (ProcessT m ext c) -> col (ProcessT m ext c)))
        (col c)
rpSwitch r sfs =
    ggSwitch
        (\(ParStep (PluralStep _ sfs') IDStep) -> sfs')
        (ParStep (PluralStep r sfs) IDStep)
        (\sfs' tr -> next r (tr sfs'))
  where
    next ::
        (Monad m, Tv.Traversable col) =>
        (forall sf. (b -> col sf -> col (ext, sf))) ->
        col (ProcessT m ext c) ->
        ProcessT m
            (b, Event (col (ProcessT m ext c) -> col (ProcessT m ext c)))
            (col c)
    next r' sfs' =
        dggSwitch
            (\(ParStep (PluralStep _ sfs'') IDStep) -> sfs'')
            (ParStep (PluralStep r' sfs') IDStep)
            (\sfs'' _ -> rpSwitch r' sfs'')


rpSwitchB ::
    (Monad m, Tv.Traversable col) =>
    col (ProcessT m b c) ->
    ProcessT m
        (b, Event (col (ProcessT m b c) -> col (ProcessT m b c)))
        (col c)
rpSwitchB = rpSwitch broadcast


drpSwitch ::
    (Monad m, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessT m ext c) ->
    ProcessT m
        (b, Event (col (ProcessT m ext c) -> col (ProcessT m ext c)))
        (col c)
drpSwitch r sfs =
    dggSwitch
        (\(ParStep (PluralStep _ sfs') IDStep) -> sfs')
        (ParStep (PluralStep r sfs) IDStep)
        (\sfs' tr -> drpSwitch r (tr sfs'))

drpSwitchB ::
    (Monad m, Tv.Traversable col) =>
    col (ProcessT m b c) ->
    ProcessT m
        (b, Event (col (ProcessT m b c) -> col (ProcessT m b c)))
        (col c)
drpSwitchB = drpSwitch broadcast


--
-- Unsafe primitives
--

-- | Repeatedly call `p`.
--
-- How many times `p` is called is indefinite.
-- So `p` must satisfy the equation below;
--
-- @p &&& (p >>> arr null) === p &&& arr (const True)@
--
-- where
--
-- @null = getAll . foldMap (\_ -> All False)@
unsafeExhaust ::
    (Monad m, Fd.Foldable f) =>
    (b -> m (f c)) ->
    ProcessT m b (Event c)
unsafeExhaust p =
    go >>> fork
  where
    go = ProcessT {
        paFeed = \x -> do {y <- p x; return (Event y, go)},
        paSweep = \x -> do {y <- p x; return (if nullFd y then Nothing else Just (Event y), go)},
        paSuspend = const NoEvent
      }

    fork = repeatedly $ await >>= Fd.mapM_ yield

    nullFd = getAll . Fd.foldMap (\_ -> All False)


--
-- Running
--

--
-- Running Monad (To be exported)
--
data RunInfo i o m = RunInfo {
    freezeRI :: !(ProcessT m i o),
    getInputRI :: !i,
    getPaddingRI :: !i,
    getPhaseRI :: !Phase
  }

type RM i o m = StateT (RunInfo i o m) m

runRM ::
    Monad m' =>
    ProcessT m (Event i) o ->
    StateT (RunInfo (Event i) o m) m' x ->
    m' x
runRM pa mx =
    evalStateT mx $
        RunInfo {
            freezeRI = pa,
            getInputRI = NoEvent,
            getPaddingRI = NoEvent,
            getPhaseRI = Sweep
          }



feed_ ::
    (Monad m, MonadState (RunInfo i o m') m) =>
    i -> i -> m Bool
feed_ input padding =
  do
    ph <- gets getPhaseRI
    if ph == Suspend
        then
          do
            ri <- get
            put $ ri {
                getInputRI = input,
                getPaddingRI = padding,
                getPhaseRI = Feed
              }
            return True
        else
            return False

feedR ::
    (Monad m, MonadState (RunInfo (Event i) o m') m) =>
    i -> m Bool
feedR x = feed_ (Event x) NoEvent


freeze ::
    Monad m =>
    RM i o m (ProcessT m i o)
freeze = gets freezeRI

sweepR ::
    Monad m =>
    RM i o m o
sweepR =
  do
    pa <- freeze
    ph <- gets getPhaseRI
    ri <- get
    case ph of
      Feed ->
        do
            x <- gets getInputRI
            (y, pa') <- lift $ feed pa x
            put $ ri {
                freezeRI = pa',
                getPhaseRI = Sweep
              }
            return y
      Sweep ->
        do
            x <- gets getPaddingRI
            (my, pa') <- lift $ sweep pa x
            put $ ri {
                freezeRI = pa',
                getPhaseRI = if isJust my then Sweep else Suspend
              }
            return $ fromMaybe (suspend pa x) my
      Suspend ->
        do
            x <- gets getPaddingRI
            return $ suspend pa x


sweepAll ::
    (Monad m, Monad m') =>
    (forall p. RM i (Event o) m p -> m' p) ->
    (o -> m' ()) ->
    ContT Bool m' ()
sweepAll lft outpre =
    callCC $ \sus -> forever $ cond sus >> body
  where
    cond sus =
      do
        ph <- lift $ lft $ gets getPhaseRI
        if ph == Suspend then sus () else return ()
    body =
      do
        evx <- lift $ lft $ sweepR
        case evx
          of
            Event x ->
              do
                lift $ outpre x
            NoEvent ->
                return ()
            End ->
                breakCont False

breakCont :: Monad m => r -> ContT r m a
breakCont = ContT . const . return


-- | Run a machine.
runT ::
    (Monad m, Fd.Foldable f) =>
    (c -> m ()) ->
    ProcessT m (Event b) (Event c) ->
    f b -> m ()
runT outpre0 pa0 xs =
    runRM pa0 $
      do
        _ <- evalContT $
          do
            -- Sweep initial events.
            sweepAll id outpre

            -- Feed values
            Fd.mapM_ feedSweep xs

            return True

        -- Terminate.
        _ <- feed_ End End
        _ <- evalContT $ sweepAll id outpre >> return True
        return ()
  where
    feedSweep x =
      do
        _ <- lift $ feedR x
        sweepAll id outpre

    outpre = lift . outpre0

type Builder b = F.F ((,) b)

putB :: b -> Builder b ()
putB x = F.liftF (x, ())

bToList :: Builder b a -> [b]
bToList x = build $ \cons nil -> F.runF x (const nil) (uncurry cons)

-- | Run a machine discarding all results.
runT_ ::
    (Monad m, Fd.Foldable f) =>
    ProcessT m (Event a) (Event b) ->
    f a -> m ()
runT_ pa l =
    runT (const $ return ()) pa l

run ::
    Fd.Foldable f =>
    ProcessT Identity (Event a) (Event b) ->
    f a -> [b]
run pa = bToList . runT putB (fit lift pa)

run_ ::
    (Fd.Foldable f, ArrowApply a) =>
    ProcessA a (Event b) (Event c) ->
    a (f b) ()
run_ pa = proc l -> case runT_ pa l of {ArrowMonad f -> f} -<< ()

lftRM :: (Monad m, Monad m') =>
    (forall p. m p -> m' p) ->
    RM i o m a ->
    StateT (RunInfo i o m) m' a
lftRM lft' st = StateT $ \s -> lft' $ runStateT st s


-- | Execute until an input consumed and the machine suspends.
--
-- During the execution, the machine may yield values or stops.
-- It can be handled by two callbacks.
--
-- In some case the machine failed to consume the input value.
-- If so, the value is passed to the termination callback.
stepRun ::
    (Monad m, Monad m') =>
    (forall p. m p -> m' p) -- ^ Lifting function (pass `id` if m' ~ m)
      ->
    (b -> m' ()) -- ^ Callback on every output value.
      ->
    (Maybe a -> m' ()) -- ^ Callback on termination.
      ->
    ProcessT m (Event a) (Event b)  -- ^ The machine to run.
      ->
    a -- ^ The argument to the machine.
      ->
    m' (ProcessT m (Event a) (Event b))
stepRun lft yd stp pa0 x =
  do
    pa <- runRM pa0 $
      do
        csmd <- evalContT $
          do
            sweepAll (lftRM lft) (lift . yd)
            return True
        if csmd
          then do
            ct <- evalContT $
              do
                _ <- lift $ feedR x
                sweepAll (lftRM lft) (lift . yd)
                return True
            if ct
              then return ()
              else lift $ stp $ Nothing
          else
            lift $ stp $ Just x
        pa <- lftRM lft freeze
        return pa
    return pa


-- | Execute until an output produced.
--
-- During the execution, the machine may await values or stops.
-- It can be handled by two callbacks.
--
-- If the machine stops without producing any value,
-- The first element of the return tuple is `Nothing`.
stepYield ::
    (Monad m, Monad m') =>
    (forall p. m p -> m' p)  -- ^ Lifting function (pass `id` if m' ~ m)
      ->
    m' a -- ^ Callback on input value request.
      ->
    m' () -- ^ Callback on termination
      ->
    ProcessT m (Event a) (Event b) -- ^ The machine to run.
      ->
    m' (Maybe b, ProcessT m (Event a) (Event b))
stepYield lft aw stp pa0 = runRM pa0 $
  do
    r <- go False
    pa <- lftRM lft freeze
    return (r, pa)

  where
    go csmd =
        lftRM lft sweepR >>= handleEv csmd

    handleEv _ (Event y) =
        return $ Just y

    handleEv True NoEvent =
        return Nothing

    handleEv False NoEvent =
      do
        x <- lift $ aw
        _ <- lftRM lft $ feedR x
        go True

    handleEv _ End =
        lift stp >> return Nothing
