{-# LANGUAGE Trustworthy #-} -- Safe if eliminate GeneralizedNewtypeInstance
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE KindSignatures #-}

module
    Control.Arrow.Machine.Types
      (
        -- * Stream transducer type
        ProcessA(),

        -- * Event type and utility
        Occasional' (..),
        Occasional (..),
        Event (),
        condEvent,
        filterEvent,
        filterJust,
        filterLeft,
        filterRight,
        evMap,

        -- * Coroutine monad
        -- | Procedural coroutine monad that can await or yield values.
        --
        -- Coroutines can be encoded to machines by `constructT` or so on and
        -- then put into `ProcessA` compositions.
        PlanT(..),
        Plan,

        await,
        yield,
        stop,
        catchP,

        stopped,
        muted,

        -- * Constructing machines from plans
        constructT,
        repeatedlyT,

        construct,
        repeatedly,

        -- * Running machines (at once)
        run,
        runOn,
        run_,

        -- * Running machines (step-by-step)
        ExecInfo(..),
        stepRun,
        stepYield,

        -- * Primitive machines - switches
        -- | Switches inspired by Yampa library.
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
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer hiding ((<>))
import Control.Monad.Identity
import Control.Applicative
import Data.Foldable as Fd
import Data.Traversable as Tv
import Data.Semigroup (Semigroup, (<>))
import Data.Maybe (fromMaybe, isNothing, isJust)
import qualified Control.Monad.Trans.Free as F
import qualified Control.Monad.Trans.Free.Church as F
import Control.Arrow.Machine.ArrowUtil
import GHC.Exts (build)


-- | To get multiple outputs by one input, the `Phase` parameter is introduced.
--
-- Once a value `Feed`ed, the machine is `Sweep`ed until it `Suspend`s.
data Phase = Feed | Sweep | Suspend deriving (Eq, Show)

instance
    Monoid Phase
  where
    mempty = Sweep

    mappend Feed _ = Feed
    mappend _ Feed = Feed
    mappend Suspend _ = Suspend
    mappend _ Suspend = Suspend
    mappend Sweep Sweep = Sweep


type ProcType a b c = ProcessA a b c

class Stepper a b c s | s -> a, s -> b, s -> c
  where
    feed :: s -> a b (c, s)
    sweep :: s -> a b (Maybe c, s)
    suspend :: s -> b -> c

-- | The stream transducer arrow.
--
-- To construct `ProcessA` instances, use `Control.Arrow.Machine.Plan.Plan`,
-- `arr`, functions declared in `Control.Arrow.Machine.Utils`,
-- or arrow combinations of them.
--
-- See an introduction at "Control.Arrow.Machine" documentation.
data ProcessA a b c = ProcessA {
    paFeed :: a b (c, ProcessA a b c),
    paSweep :: a b (Maybe c, ProcessA a b c),
    paSuspend :: !(b -> c)
  }

instance
    Stepper a b c (ProcessA a b c)
  where
    feed = paFeed
    sweep = paSweep
    suspend = paSuspend

toProcessA ::
    (ArrowApply a, Stepper a b c s) =>
    s -> ProcessA a b c
toProcessA s = ProcessA {
    paFeed = feed s >>> arr (second toProcessA),
    paSweep = sweep s >>> arr (second toProcessA),
    paSuspend = suspend s
  }
{-# INLINE[2] toProcessA  #-}

-- For internal use
class
    (Applicative f, Monad f) => ProcessHelper f
  where
    step ::
        (ArrowApply a, Stepper a b c s) =>
        s -> a b (f c, s)
    helperToMaybe :: f a -> Maybe a
    weakly :: a -> f a
    compositeStep ::
        (ArrowApply a, Stepper a b p s1, Stepper a p c s2) =>
        s1 -> s2 ->
        a b (f c, s1, s2)


instance
    ProcessHelper Identity
  where
    step pa = feed pa >>> first (arr Identity)
    helperToMaybe = Just . runIdentity
    weakly = Identity
    compositeStep sf test = proc x ->
      do
        (y, sf') <- feed sf -< x
        (z, test') <- feed test -< y
        returnA -< (return z, sf', test')

instance
    ProcessHelper Maybe
  where
    step = sweep
    helperToMaybe = id
    weakly _ = Nothing
    compositeStep sf0 test0 = proc x ->
      do
        let y = suspend sf0 x
        (mt, test') <- sweep test0 -< y
        (case mt of
            Just t -> arr $ const (Just t, sf0, test')
            Nothing -> cont sf0 test')
                -<< x
      where
        cont sf test = proc x ->
          do
            (my, sf') <- sweep sf -< x
            (case my of
                Just y -> cont2 y sf' test
                Nothing -> arr $ const (Nothing, sf', test))
                    -<< x
        cont2 y sf test = proc _ ->
          do
            (t, test') <- feed test -< y
            returnA -< (Just t, sf, test')

makePA ::
    Arrow a =>
    (forall f. ProcessHelper f =>
        a b (f c, ProcessA a b c)) ->
    (b -> c) ->
    ProcessA a b c
makePA h !sus = ProcessA {
    paFeed = h >>> first (arr runIdentity),
    paSweep = h,
    paSuspend = sus
  }


data CompositeStep (a :: * -> * -> *) b c s1 s2 = CompositeStep s1 s2

instance
    (ArrowApply a, Stepper a b p s1, Stepper a p c s2) =>
    Stepper a b c (CompositeStep a b c s1 s2)
  where
    feed (CompositeStep s1 s2) =
        compositeStep s1 s2 >>>
            arr (\(fz, s1', s2') -> (runIdentity $ fz, CompositeStep s1' s2'))
    sweep (CompositeStep s1 s2) =
        compositeStep s1 s2 >>>
            arr (\(fz, s1', s2') -> (fz, CompositeStep s1' s2'))
    suspend (CompositeStep s1 s2) =
        suspend s2 . suspend s1


data IDStep a b c
  where
    IDStep :: IDStep (a :: * -> * -> *) b b

instance
    ArrowApply a => Stepper a b c (IDStep a b c)
  where
    feed IDStep = Cat.id &&& arr (const IDStep)
    sweep IDStep = arr (const (Nothing, IDStep))
    suspend IDStep = id

newtype ArrStep (a :: * -> * -> *) b c = ArrStep (b -> c)

instance
    ArrowApply a => Stepper a b c (ArrStep a b c)
  where
    feed (ArrStep f) = arr $ \x -> (f x, ArrStep f)
    sweep (ArrStep f) = arr $ const (Nothing, ArrStep f)
    suspend (ArrStep f) = f


data ParStep a b c s1 s2
  where
    ParStep ::
        (Stepper a b1 c1 s1, Stepper a b2 c2 s2) =>
        s1 -> s2 ->
        ParStep a (b1, b2) (c1, c2) s1 s2

instance
    ArrowApply a => Stepper a b c (ParStep a b c s1 s2)
  where
    feed (ParStep f g) = proc (x1, x2) ->
      do
        (y1, f') <- feed f -< x1
        (y2, g') <- feed g -< x2
        returnA -< ((y1, y2), ParStep f' g')
    sweep (ParStep f g) = proc (x1, x2) ->
      do
        (my1, f') <- sweep f -< x1
        (my2, g') <- sweep g -< x2
        let y1 = fromMaybe (suspend f' x1) my1 -- suspend f ?
            y2 = fromMaybe (suspend g' x2) my2
            r = if (isNothing my1 && isNothing my2) then Nothing else Just (y1, y2)
        returnA -< (r, ParStep f' g')
    suspend (ParStep f g) = suspend f *** suspend g


-- |Natural transformation
fit ::
    (ArrowApply a, ArrowApply a') =>
    (forall p q. a p q -> a' p q) ->
    ProcessA a b c -> ProcessA a' b c
fit f pa =
    arr Identity >>>
    fitW runIdentity (\ar -> arr runIdentity >>> f ar) pa

-- |Experimental: more general fit.
--
-- Should w be a comonad?
fitW :: (ArrowApply a, ArrowApply a', Functor w) =>
    (forall p. w p -> p) ->
    (forall p q. a p q -> a' (w p) q) -> 
    ProcessA a b c -> ProcessA a' (w b) c
fitW extr f pa = makePA
    (f (step pa) >>> arr (second $ fitW extr f))
    (extr >>> suspend pa)


instance
    ArrowApply a => Profunctor (ProcessA a)
  where
    dimap = dimapProc
    {-# INLINE dimap #-}

dimapProc ::
    ArrowApply a =>
    (b->c)->(d->e)->
    ProcType a c d -> ProcType a b e
dimapProc f g pa = makePA
    (arr f >>> step pa >>> (arr (fmap g) *** arr (dimapProc f g)))
    (dimap f g (suspend pa))

{-# NOINLINE dimapProc #-}


instance
    ArrowApply a => Functor (ProcessA a i)
  where
    fmap = rmap

instance
    ArrowApply a => Applicative (ProcessA a i)
  where
    pure = arr . const
    pf <*> px = (pf &&& px) >>> arr (uncurry ($))


instance
    ArrowApply a => Cat.Category (ProcessA a)
  where
    id = toProcessA IDStep
    {-# INLINE id #-}

    g . f = toProcessA (compositeProc f g)
    {-# INLINE (.) #-}


instance
    ArrowApply a => Arrow (ProcessA a)
  where
    arr = toProcessA . ArrStep
    {-# INLINE arr #-}

    first pa = toProcessA (ParStep pa IDStep)
    {-# INLINE first #-}

    second pa = toProcessA (ParStep IDStep pa)
    {-# INLINE second #-}

    pa *** pb = toProcessA (ParStep pa pb)
    {-# INLINE (***) #-}


-- |Composition is proceeded by the backtracking strategy.
compositeProc ::
    (ArrowApply a, Stepper a b d s1, Stepper a d c s2) =>
    s1 -> s2 -> CompositeStep a b c s1 s2
compositeProc = CompositeStep
{-# INLINE[1] compositeProc #-}

-- rules
{-# RULES
"ProcessA: toProcessA/*[1]"
    forall f g. compositeProc (toProcessA f) g = compositeProc f g
"ProcessA: toProcessA/*[2]"
    forall f g. compositeProc f (toProcessA g) = compositeProc f g
  #-}


instance
    ArrowApply a => ArrowChoice (ProcessA a)
  where
    left pa0 = makePA
        (proc eth -> sweep' pa0 eth -<< ())
        (left $ suspend pa0)
      where
        sweep' pa (Left x) = proc () ->
          do
            (my, pa') <- step pa -< x
            returnA -< (Left <$> my, left pa')
        sweep' pa (Right d) = proc () ->
            returnA -< (weakly (Right d), left pa)

instance
    ArrowApply a => ArrowLoop (ProcessA a)
  where
    loop pa =
        makePA
            (proc x ->
              do
                (hyd, pa') <- step pa -< (x, loopSusD x)
                returnA -< (fst <$> hyd, loop pa'))
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
    Semigroup a => Monoid (Event a)
  where
    mempty = End
    Event x `mappend` Event y = Event (x <> y)
    Event x `mappend` _ = Event x
    _ `mappend` Event y = Event y
    NoEvent `mappend` _ = NoEvent
    _ `mappend` NoEvent = NoEvent
    _ `mappend` _ = End



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
    noEvent :: a
    end :: a


instance
    (Occasional' a, Occasional' b) => Occasional' (a, b)
  where
    collapse (x, y) = collapse x `mappend` collapse y

instance
    (Occasional a, Occasional b) => Occasional (a, b)
  where
    noEvent = (noEvent, noEvent)
    end = (end, end)

instance
    Occasional' (Event a)
  where
    collapse = (() <$)

instance
    Occasional (Event a)
  where
    noEvent = NoEvent
    end = End


condEvent :: Bool -> Event a -> Event a
condEvent _ End = End
condEvent True ev = ev
condEvent False _ = NoEvent

filterEvent :: (a -> Bool) -> Event a -> Event a
filterEvent cond ev@(Event x) = condEvent (cond x) ev
filterEvent _ ev = ev

filterJust :: Event (Maybe a) -> Event a
filterJust (Event (Just x)) = Event x
filterJust (Event Nothing) = NoEvent
filterJust NoEvent = NoEvent
filterJust End = End

filterLeft :: Event (Either a b) -> Event a
filterLeft = filterJust . fmap (either Just (const Nothing))

filterRight :: Event (Either a b) -> Event b
filterRight = filterJust . fmap (either (const Nothing) Just)

-- | Alias of "arr . fmap"
--
-- While "ProcessA a (Event b) (Event c)" means a transducer from b to c,
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


stopped ::
    (ArrowApply a, Occasional c) => ProcessA a b c
stopped = arr (const end)


muted ::
    (ArrowApply a, Occasional' b, Occasional c) => ProcessA a b c
muted = proc x ->
  do
    ed <- construct (forever await `catchP` yield ()) -< collapse x
    rSwitch (arr $ const noEvent) -< ((), stopped <$ ed)



data PlanF i o a where
  AwaitPF :: (i->a) -> a -> PlanF i o a
  YieldPF :: o -> a -> PlanF i o a
  StopPF :: PlanF i o a

instance (Functor (PlanF i o)) where
  fmap g (AwaitPF f ff) = AwaitPF (g . f) (g ff)
  fmap g (YieldPF x r) = YieldPF x (g r)
  fmap _ StopPF = StopPF

newtype PlanT i o m a =
    PlanT { freePlanT :: F.FT (PlanF i o) m a }
  deriving
    (Functor, Applicative, Monad, MonadTrans,
     Alternative)
    -- , MonadError, MonadReader, MonadCatch, MonadThrow, MonadIO, MonadCont
type Plan i o a = forall m. Monad m => PlanT i o m a

instance
    MonadReader r m => MonadReader r (PlanT i o m)
  where
    ask = PlanT ask
    local f (PlanT pl) = PlanT $ local f pl

instance
    MonadWriter w m => MonadWriter w (PlanT i o m)
  where
    tell = PlanT . tell
    listen = PlanT . listen . freePlanT
    pass = PlanT . pass . freePlanT

instance
    MonadState s m => MonadState s (PlanT i o m)
  where
    get = PlanT get
    put = PlanT . put

instance
    (Monad m, Alternative m) => MonadPlus (PlanT i o m)
  where
    mzero = stop
    mplus = catchP

yield :: o -> Plan i o ()
yield x = PlanT . F.liftF $ YieldPF x ()

await :: Plan i o i
await = PlanT $ F.FT $ \pr free -> free id (AwaitPF pr (free pr StopPF))

stop :: Plan i o a
stop = PlanT $ F.liftF $ StopPF


catchP:: Monad m =>
    PlanT i o m a -> PlanT i o m a -> PlanT i o m a

catchP (PlanT pl) cont0 =
    PlanT $ F.FT $ \pr free ->
        F.runFT
            pl
            (pr' pr)
            (free' cont0 pr free)
  where
    pr' pr = pr

    free' ::
        Monad m =>
        PlanT i o m a ->
        (a -> m r) ->
        (forall x. (x -> m r) -> PlanF i o x -> m r) ->
        (y -> m r) ->
        (PlanF i o y) ->
        m r
    free' (PlanT cont) pr free _ StopPF =
        F.runFT cont pr free
    free' (PlanT cont) pr free r (AwaitPF f ff) =
        free
            (either (\_ -> F.runFT cont pr free) r)
            (AwaitPF (Right . f) (Left ff))
    free' _ _ free r pf =
        free r pf




constructT ::
    (Monad m, ArrowApply a) =>
    (forall b. m b -> a () b) ->
    PlanT i o m r ->
    ProcessA a (Event i) (Event o)
constructT = constructT'


constructT' ::
    forall a m i o r.
    (Monad m, ArrowApply a) =>
    (forall b. m b -> a () b) ->
    PlanT i o m r ->
    ProcessA a (Event i) (Event o)
constructT' fit0 (PlanT pl0) = prependProc $ F.runFT pl0 pr free
  where
    fit' :: (b -> m c) -> a b c
    fit' fmy = proc x -> fit0 (fmy x) -<< ()

    prependProc ::
        m (Event o, ProcessA a (Event i) (Event o)) ->
        ProcessA a (Event i) (Event o)
    prependProc mr = ProcessA {
        paFeed = proc ex -> do { r <- fit0 mr -< (); prependFeed r -<< ex} ,
        paSweep = proc ex -> do { r <- fit0 mr -< (); prependSweep r -<< ex},
        paSuspend = const NoEvent
      }

    prependFeed (Event x, pa) = arr $ const (Event x, pa)
    prependFeed (NoEvent, pa) = feed pa
    prependFeed (End, _) = arr $ const (End, stopped)

    prependSweep (Event x, pa) = arr $ const (Just (Event x), pa)
    prependSweep (NoEvent, pa) = sweep pa
    prependSweep (End, _) = arr $ const (Just End, stopped)

    pr _ = return (End, stopped)

    free ::
        (x -> m (Event o, ProcessA a (Event i) (Event o)))->
        PlanF i o x ->
        m (Event o, ProcessA a (Event i) (Event o))
    free r (YieldPF y cont) =
        return (Event y, prependProc (r cont))
    free r pl@(AwaitPF f ff) =
        return (NoEvent, awaitProc fma)
      where
        fma (Event x) = r (f x)
        fma NoEvent = free r pl
        fma End = r ff
    free _ StopPF =
        return (End, stopped)

    awaitProc fma = ProcessA {
        paFeed = fit' fma,
        paSweep = fit' fma >>> first eToM,
        paSuspend = const NoEvent
      }

    eToM :: a (Event b) (Maybe (Event b))
    eToM = arr eToMpure
    eToMpure NoEvent = Nothing
    eToMpure e = Just e


repeatedlyT :: (Monad m, ArrowApply a) =>
              (forall b. m b -> a () b) ->
              PlanT i o m r ->
              ProcessA a (Event i) (Event o)

repeatedlyT f pl = constructT f $ forever pl


-- for pure
construct :: ArrowApply a =>
             Plan i o t ->
             ProcessA a (Event i) (Event o)
construct pl = constructT (ary0 unArrowMonad) pl

repeatedly :: ArrowApply a =>
              Plan i o t ->
              ProcessA a (Event i) (Event o)
repeatedly pl = construct $ forever pl


--
-- Switches
--
switch ::
    ArrowApply a =>
    ProcessA a b (c, Event t) ->
    (t -> ProcessA a b c) ->
    ProcessA a b c
switch sf k = ggSwitch (const ()) sf (\() -> k)


dSwitch ::
    ArrowApply a =>
    ProcessA a b (c, Event t) ->
    (t -> ProcessA a b c) ->
    ProcessA a b c
dSwitch sf k = dggSwitch (const ()) sf (\() -> k)


rSwitch ::
    ArrowApply a => ProcessA a b c ->
    ProcessA a (b, Event (ProcessA a b c)) c
rSwitch p = rSwitch' (p *** Cat.id) >>> arr fst
  where
    rSwitch' pid = kSwitch pid test $ \_ p' -> rSwitch'' (p' *** Cat.id)
    rSwitch'' pid = dkSwitch pid test $ \s _ -> rSwitch' s
    test = proc (_, (_, r)) -> returnA -< r


drSwitch ::
    ArrowApply a => ProcessA a b c ->
    ProcessA a (b, Event (ProcessA a b c)) c

drSwitch p =  drSwitch' (p *** Cat.id)
  where
    drSwitch' pid = dSwitch pid $ \p' -> drSwitch' (p' *** Cat.id)


kSwitch ::
    ArrowApply a =>
    ProcessA a b c ->
    ProcessA a (b, c) (Event t) ->
    (ProcessA a b c -> t -> ProcessA a b c) ->
    ProcessA a b c
kSwitch sf test =
    ggSwitch
        (\(CompositeStep _ (CompositeStep (ParStep IDStep sf') _)) -> sf')
        (CompositeStep (ArrStep (id &&& id))
           (CompositeStep (ParStep IDStep sf) (arr snd &&& test)))


dkSwitch ::
    ArrowApply a =>
    ProcessA a b c ->
    ProcessA a (b, c) (Event t) ->
    (ProcessA a b c -> t -> ProcessA a b c) ->
    ProcessA a b c
dkSwitch sf test =
    dggSwitch
        (\(CompositeStep _ (CompositeStep (ParStep IDStep sf') _)) -> sf')
        (CompositeStep (ArrStep (id &&& id))
           (CompositeStep (ParStep IDStep sf) (arr snd &&& test)))

ggSwitch ::
    (ArrowApply a, Stepper a b (c, Event t) sWhole) =>
    (sWhole -> s) ->
    sWhole ->
    (s -> t -> ProcessA a b c) ->
    ProcessA a b c
ggSwitch picker whole k = makePA
    (proc x ->
      do
        let
        (hyevt, whole') <- step whole -<< x
        let hy = fst <$> hyevt
            hevt = snd <$> hyevt
        (case (helperToMaybe hevt)
          of
            Just (Event t) -> step (k (picker whole') t)
            _ -> arr $ const (hy, ggSwitch picker whole' k))
                -<< x)
    (arr fst . suspend whole)

dggSwitch ::
    (ArrowApply a, Stepper a b (c, Event t) sWhole) =>
    (sWhole -> s) ->
    sWhole ->
    (s -> t -> ProcessA a b c) ->
    ProcessA a b c
dggSwitch picker whole k = makePA
    (proc x ->
      do
        let
        (hyevt, whole') <- step whole -<< x
        let hy = fst <$> hyevt
            hevt = snd <$> hyevt
        (case (helperToMaybe hevt)
          of
            Just (Event t) -> arr $ const (hy, k (picker whole') t)
            _ -> arr $ const (hy, dggSwitch picker whole' k))
                -<< x)
    (arr fst . suspend whole)

gSwitch ::
    ArrowApply a =>
    ProcessA a b (p, r) ->
    ProcessA a p q ->
    ProcessA a (q, r) (c, Event t) ->
    (ProcessA a p q -> t -> ProcessA a b c) ->
    ProcessA a b c
gSwitch pre sf post =
    ggSwitch
        (\(CompositeStep _ (CompositeStep (ParStep sf' IDStep) _)) -> sf')
        (CompositeStep pre (CompositeStep (ParStep sf IDStep) post))

dgSwitch ::
    ArrowApply a =>
    ProcessA a b (p, r) ->
    ProcessA a p q ->
    ProcessA a (q, r) (c, Event t) ->
    (ProcessA a p q -> t -> ProcessA a b c) ->
    ProcessA a b c
dgSwitch pre sf post =
    dggSwitch
        (\(CompositeStep _ (CompositeStep (ParStep sf' IDStep) _)) -> sf')
        (CompositeStep pre (CompositeStep (ParStep sf IDStep) post))

broadcast ::
    Functor col =>
    b -> col sf -> col (b, sf)
broadcast x sfs = fmap (\sf -> (x, sf)) sfs

par ::
    (ArrowApply a, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessA a ext c) ->
    ProcessA a b (col c)
par r sfs = toProcessA (PluralStep r sfs)

parB ::
    (ArrowApply a, Tv.Traversable col) =>
    col (ProcessA a b c) ->
    ProcessA a b (col c)
parB = par broadcast


data PluralStep ext col a b c
  where
    PluralStep ::
        (forall sf. (b -> col sf -> col (ext, sf))) ->
        (col (ProcessA a ext c)) ->
        PluralStep ext col a b c


instance
    (ArrowApply a, Tv.Traversable col) =>
    Stepper a b (col c) (PluralStep ext col a b c)
  where
    feed (PluralStep r sfs) = parCore r sfs >>> arr (runIdentity *** PluralStep r)
    sweep (PluralStep r sfs) = parCore r sfs >>> arr (id *** PluralStep r)
    suspend (PluralStep r sfs) = suspendAll r sfs

suspendAll ::
    (ArrowApply a, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessA a ext c) ->
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
    (ArrowApply a, Tv.Traversable col, ProcessHelper h) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessA a ext c) ->
    a b (h (col c), col (ProcessA a ext c))

parCore r sfs = proc x ->
  do
    let input = r x sfs
    ret <- unwrapArrow (Tv.sequenceA (fmap (WrapArrow . app') input)) -<< ()
    let zs = traverseResult $ fmap fst ret
        sfs' = fmap snd ret
    returnA -< (zs, sfs')
  where
    app' (y, sf) = proc () ->
      do
        (hz, sf') <- step sf -< y
        returnA -< ((hz, suspend sf' y), sf')


pSwitch ::
    (ArrowApply a, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessA a ext c) ->
    ProcessA a (b, col c) (Event mng) ->
    (col (ProcessA a ext c) -> mng -> ProcessA a b (col c)) ->
    ProcessA a b (col c)
pSwitch r sfs test =
    ggSwitch
        (\(CompositeStep _
            (CompositeStep (ParStep IDStep (PluralStep _ sfs')) _)) -> sfs')
        (CompositeStep (ArrStep (id &&& id))
            (CompositeStep (ParStep IDStep (PluralStep r sfs)) (arr snd &&& test)))

pSwitchB ::
    (ArrowApply a, Tv.Traversable col) =>
    col (ProcessA a b c) ->
    ProcessA a (b, col c) (Event mng) ->
    (col (ProcessA a b c) -> mng -> ProcessA a b (col c)) ->
    ProcessA a b (col c)
pSwitchB = pSwitch broadcast

dpSwitch ::
    (ArrowApply a, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessA a ext c) ->
    ProcessA a (b, col c) (Event mng) ->
    (col (ProcessA a ext c) -> mng -> ProcessA a b (col c)) ->
    ProcessA a b (col c)
dpSwitch r sfs test =
    dggSwitch
        (\(CompositeStep _
            (CompositeStep (ParStep IDStep (PluralStep _ sfs')) _)) -> sfs')
        (CompositeStep (ArrStep (id &&& id))
            (CompositeStep (ParStep IDStep (PluralStep r sfs)) (arr snd &&& test)))

dpSwitchB ::
    (ArrowApply a, Tv.Traversable col) =>
    col (ProcessA a b c) ->
    ProcessA a (b, col c) (Event mng) ->
    (col (ProcessA a b c) -> mng -> ProcessA a b (col c)) ->
    ProcessA a b (col c)
dpSwitchB = dpSwitch broadcast

rpSwitch ::
    (ArrowApply a, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessA a ext c) ->
    ProcessA a
        (b, Event (col (ProcessA a ext c) -> col (ProcessA a ext c)))
        (col c)
rpSwitch r sfs =
    ggSwitch
        (\(ParStep (PluralStep _ sfs') IDStep) -> sfs')
        (ParStep (PluralStep r sfs) IDStep)
        (\sfs' tr -> next r (tr sfs'))
  where
    next ::
        (ArrowApply a, Tv.Traversable col) =>
        (forall sf. (b -> col sf -> col (ext, sf))) ->
        col (ProcessA a ext c) ->
        ProcessA a
            (b, Event (col (ProcessA a ext c) -> col (ProcessA a ext c)))
            (col c)
    next r' sfs' =
        dggSwitch
            (\(ParStep (PluralStep _ sfs'') IDStep) -> sfs'')
            (ParStep (PluralStep r' sfs') IDStep)
            (\sfs'' _ -> rpSwitch r' sfs'')


rpSwitchB ::
    (ArrowApply a, Tv.Traversable col) =>
    col (ProcessA a b c) ->
    ProcessA a
        (b, Event (col (ProcessA a b c) -> col (ProcessA a b c)))
        (col c)
rpSwitchB = rpSwitch broadcast


drpSwitch ::
    (ArrowApply a, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessA a ext c) ->
    ProcessA a
        (b, Event (col (ProcessA a ext c) -> col (ProcessA a ext c)))
        (col c)
drpSwitch r sfs =
    dggSwitch
        (\(ParStep (PluralStep _ sfs') IDStep) -> sfs')
        (ParStep (PluralStep r sfs) IDStep)
        (\sfs' tr -> drpSwitch r (tr sfs'))

drpSwitchB ::
    (ArrowApply a, Tv.Traversable col) =>
    col (ProcessA a b c) ->
    ProcessA a
        (b, Event (col (ProcessA a b c) -> col (ProcessA a b c)))
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
    (ArrowApply a, Fd.Foldable f) =>
    a b (f c) ->
    ProcessA a b (Event c)
unsafeExhaust p =
    go >>> fork
  where
    go = ProcessA {
        paFeed = p >>> arr (\y -> (Event y, go)),
        paSweep = p >>> arr (\y -> (if nullFd y then Nothing else Just (Event y), go)),
        paSuspend = const NoEvent
      }

    fork = repeatedly $ await >>= Fd.mapM_ yield

    nullFd = getAll . Fd.foldMap (\_ -> All False)



--
-- Running
--
--
-- Utilities
--
while_ ::
    Monad m =>
    m Bool -> m a -> m ()
while_ cond body =
  do
    b <- cond
    if b
        then body >> while_ cond body
        else return ()

-- | Monoid wrapper
data WithEnd r = WithEnd {
    getRWE :: r,
    getContWE :: !Bool
  }

instance
    Monoid r => Monoid (WithEnd r)
  where
    mempty = WithEnd mempty True
    WithEnd x True `mappend` WithEnd y b = WithEnd (x `mappend` y) b
    mx@(WithEnd _ False) `mappend` _ = mx


--
-- Running Monad (To be exported)
--
data RunInfo a i o m = RunInfo {
    freezeRI :: ProcessA a i o,
    getInputRI :: i,
    getPaddingRI :: i,
    getPhaseRI :: Phase,
    getFitRI :: forall p q. a p q -> p -> m q
  }

type RM a i o m = StateT (RunInfo a i o m) m

runRM ::
    (Monad m, ArrowApply a) =>
    (forall p q. a p q -> p -> m q) ->
    ProcessA a (Event i) o ->
    RM a (Event i) o m x ->
    m x
runRM f pa mx =
    evalStateT mx $
        RunInfo {
            freezeRI = pa,
            getInputRI = NoEvent,
            getPaddingRI = NoEvent,
            getPhaseRI = Sweep,
            getFitRI = f
          }



feed_ ::
    Monad m =>
    i -> i -> RM a i o m Bool
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
    Monad m =>
    i -> RM a (Event i) o m Bool
feedR x = feed_ (Event x) NoEvent


{-
finalizeE ::
    Monad m =>
    RM a (Event i) o m Bool
finalizeE = feed_ End End
-}

freeze ::
    Monad m =>
    RM a i o m (ProcessA a i o)
freeze = gets freezeRI


sweepR ::
    Monad m =>
    RM a i o m o
sweepR =
  do
    pa <- freeze
    ph <- gets getPhaseRI
    ri <- get
    case ph of
      Feed ->
        do
            fit0 <- gets getFitRI
            x <- gets getInputRI
            (y, pa') <- lift $ fit0 (feed pa) x
            put $ ri {
                freezeRI = pa',
                getPhaseRI = Sweep
              }
            return y
      Sweep ->
        do
            fit0 <- gets getFitRI
            x <- gets getPaddingRI
            (my, pa') <- lift $ fit0 (sweep pa) x
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
    (ArrowApply a, Monoid r, Monad m) =>
    (o->r) ->
    WriterT (WithEnd r) (RM a i (Event o) m) ()
sweepAll outpre =
        while_
            ((not . (== Suspend)) `liftM` lift (gets getPhaseRI)) $
          do
            evx <- lift sweepR
            case evx
              of
                Event x ->
                    tell (WithEnd (outpre x) True)
                NoEvent ->
                    return ()
                End ->
                    tell (WithEnd mempty False)


-- | Run a machine with results concatenated in terms of a monoid.
runOn ::
    (ArrowApply a, Monoid r, Fd.Foldable f) =>
    (c -> r) ->
    ProcessA a (Event b) (Event c) ->
    a (f b) r
runOn outpre pa0 = unArrowMonad $ \xs ->
  do
    wer <- runRM arrowMonad pa0 $ execWriterT $
      do
        -- Sweep initial events.
        (_, wer) <- listen $ sweepAll outpre

        -- Feed inputs.
        if getContWE wer
          then
            Fd.foldr feedSweep (return ()) xs
          else
            return ()

        -- Terminate.
        _ <- lift (feed_ End End)
        sweepAll outpre
    return $ getRWE wer

  where
    feedSweep x cont =
      do
        _ <- lift $ feedR x
        ((), wer) <- listen $ sweepAll outpre
        if getContWE wer then cont else return ()



newtype Builder a = Builder {
    unBuilder :: forall b. (a -> b -> b) -> b -> b
  }
instance
    Monoid (Builder a)
  where
    mempty = Builder $ \_ e -> e
    Builder g `mappend` Builder f =
        Builder $ \c e -> g c (f c e)

-- | Run a machine.
run ::
    ArrowApply a =>
    ProcessA a (Event b) (Event c) ->
    a [b] [c]
run pa =
    runOn (\x -> Builder $ \c e -> c x e) pa >>>
    arr (\b -> build (unBuilder b))

-- | Run a machine discarding all results.
run_ ::
    ArrowApply a =>
    ProcessA a (Event b) (Event c) ->
    a [b] ()
run_ pa =
    runOn (const ()) pa


-- | Represents return values and informations of step executions.
data ExecInfo fa =
    ExecInfo
      {
        yields :: fa, -- ^ Values yielded while the step.
        hasConsumed :: Bool, -- ^ True if the input value is consumed.
            --
            -- False if the machine has stopped unless consuming the input.
            --
            -- Or in the case of `stepYield`, this field become false when
            -- the machine produces a value unless consuming the input.
        hasStopped :: Bool -- ^ True if the machine has stopped at the end of the step.
      }
    deriving (Eq, Show)

instance
    Alternative f => Monoid (ExecInfo (f a))
  where
    mempty = ExecInfo empty False False
    ExecInfo y1 c1 s1 `mappend` ExecInfo y2 c2 s2 =
        ExecInfo (y1 <|> y2) (c1 || c2) (s1 || s2)


-- | Execute until an input consumed and the machine suspends.
stepRun ::
    ArrowApply a =>
    ProcessA a (Event b) (Event c) ->
    a b (ExecInfo [c], ProcessA a (Event b) (Event c))
stepRun pa0 = unArrowMonad $ \x ->
  do
    (pa, wer)  <- runRM arrowMonad pa0 $ runWriterT $
      do
        sweepAll singleton
        _ <- lift $ feedR x
        sweepAll singleton
        lift $ freeze
    return $ (retval wer, pa)

  where
    singleton x = Endo (x:)

    retval WithEnd {..} = ExecInfo {
        yields = appEndo getRWE [],
        hasConsumed = True,
        hasStopped = not getContWE
      }

-- | Execute until an output produced.
stepYield ::
    ArrowApply a =>
    ProcessA a (Event b) (Event c) ->
    a b (ExecInfo (Maybe c), ProcessA a (Event b) (Event c))
stepYield pa0 = unArrowMonad $ \x -> runRM arrowMonad pa0 $ evalStateT `flip` mempty $
  do
    go x
    r <- get
    pa <- lift freeze
    return (r, pa)

  where
    go x =
      do
        csmd <- lift $ feedR x
        modify $ \ri -> ri { hasConsumed = csmd }

        evo <- lift sweepR

        case evo
          of
            Event y ->
              do
                modify $ \ri -> ri { yields = Just y }

            NoEvent ->
              do
                csmd' <- gets hasConsumed
                if csmd' then return () else go x

            End ->
                modify $ \ri -> ri { hasStopped = True }

