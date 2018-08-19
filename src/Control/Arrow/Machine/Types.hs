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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
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
        PlanT,
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
        -- packProc,
        -- awaitProc,
        -- yieldProc,
        evolve,
        finishWith,

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
import Data.Profunctor (Profunctor, dimap, rmap, lmap)
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
import Control.Monad.Trans.Free.Church (FT ())
import qualified Control.Monad.Trans.Free.Church as FT
import Control.Monad.Free.Church (F ())
import Control.Monad.Free.Church as F
import Control.Monad.Free
import qualified Control.Monad.Free as Free
import GHC.Exts (build)


-- | To get multiple outputs by one input, the `Phase` parameter is introduced.
--
-- Once a value `Feed`ed, the machine is `Sweep`ed until it `Suspend`s.
data Phase = Feed | Sweep | Suspend deriving (Eq, Show)


    
data EvoV i o m a =
    Aw (i -> a) |
    Yd o a |
    M (m a)
  deriving Functor

data EvoF i o m a = EvoF
  {
    suspend :: i -> o,
    prepare :: i ->EvoV i o m a  
  }
  deriving Functor


newtype Evolution i o m r = Evolution 
  {
    runEvolution :: F (EvoF i o m) r
  }

instance
    Functor (Evolution i o m)
  where
    fmap g (Evolution k) = Evolution $ fmap g k

instance
    Applicative (Evolution i o m)
  where
    pure x = Evolution $ pure x
    (<*>) = ap

instance
    Monad (Evolution i o m)
  where
    Evolution mx >>= f = Evolution $ mx >>= runEvolution . f

instance
    Occasional o =>
    MonadTrans (Evolution i o)
  where
    {-# INLINE lift #-}
    lift ma = Evolution $ Free.liftF $ EvoF (const noEvent) (\_ -> M ma) 

-- | The stream transducer arrow.
--
-- To construct `ProcessT` instances, use `Control.Arrow.Machine.Plan.Plan`,
-- `arr`, functions declared in `Control.Arrow.Machine.Utils`,
-- or arrow combinations of them.
--
-- See an introduction at "Control.Arrow.Machine" documentation.
newtype ProcessT m i o = ProcessT { runProcessT :: Free (EvoF i o m) Void }

{-
toFreeEvo :: Monad m => Evolution i o m r -> Free (EvoF i o m) r
toFreeEvo evo = fst $ runState (FT.runFT (runReaderT (runEvolution evo) id) pr fr) (arr fst)
  where
    pr = return . return
    fr next fx = return $ wrap $ fmap `flip` fx $ \x -> fst $ runState (next x) (arr fst)
-}

composeEvo :: Monad m => Evolution b c m r -> ProcessT m a b -> Evolution a c m r
composeEvo q0 p0 = Evolution $ F $ \pr0 fr0 -> 
    let
        pr x _ = pr0 x
        fr q p = fr0 $ EvoF (suspend q . suspend (unFree p)) $ \i ->
            let
                susX = suspend (unFree p) i
                vQ = prepare q susX
                vP = prepare (unFree p) i
              in
                case (vP, vQ)
                  of
                    (_, M mq') -> M (($p) <$> mq')
                    (_, Yd o q') -> Yd o $ q' p
                    (M mp', Aw _) -> M (fr q <$> mp')
                    (Yd z p', Aw g) -> M (return $ g z p')
                    (Aw f, Aw _) -> Aw $ fr q . f 
      in
        F.runF (runEvolution q0) pr fr (runProcessT p0)
  where
    unFree (Pure v) = absurd v
    unFree (Free x) = x
    
evolve :: Monad m => Evolution i o m Void -> ProcessT m i o
evolve = ProcessT . F.fromF . runEvolution

finishWith :: Monad m => ProcessT m i o -> Evolution i o m a
finishWith = fmap absurd . Evolution . F.toF . runProcessT

{-
idEvo :: Monad m => Evolution i i m Void
idEvo = Evolution $ ReaderT $ \_ -> StateT $ \(ProcessT mx) ->
    fmap (\v -> (v, absurd v)) $ toF mx
-}

class
    HProfunctor p1 p2 a b c d | p1 -> b, p1 -> c, p2 -> a, p2 -> d, p1 a d -> p2, p2 b c -> p1
  where
    hdimap :: (a -> b) -> (c -> d) -> p1 r -> p2 r
    

instance
    HProfunctor (EvoV b c m) (EvoV a d m) a b c d
  where
    hdimap f _ (Aw x) = Aw (x . f)
    hdimap _ g (Yd o x) = Yd (g o) x
    hdimap _ _ (M mx) = M mx

instance
    HProfunctor (EvoF b c m) (EvoF a d m) a b c d
  where
    hdimap f g (EvoF sus prep) = EvoF (dimap f g sus) (hdimap f g . prep . f)

instance
    HProfunctor f1 f2 a b c d =>
    HProfunctor (F f1) (F f2) a b c d
  where
    hdimap f g = F.hoistF $ hdimap f g

instance
    (HProfunctor f1 f2 a b c d, Functor f1, Functor f2) =>
    HProfunctor (Free f1) (Free f2) a b c d
  where
    hdimap f g = Free.hoistFree $ hdimap f g

instance
    Monad m =>
    HProfunctor (Evolution b c m) (Evolution a d m) a b c d
  where
    hdimap f g = Evolution . hdimap f g . runEvolution


hlmap :: HProfunctor p1 p2 a b c c => (a -> b) -> p1 r -> p2 r
hlmap f = hdimap f id

hrmap :: HProfunctor p1 p2 a a b c => (b -> c) -> p1 r -> p2 r
hrmap g = hdimap id g

-- | Isomorphic to ProcessT when 'a' is ArrowApply.
type ProcessA a = ProcessT (ArrowMonad a)



-- |Natural transformation
fit ::
    (Monad m, Monad m') =>
    (forall p. m p -> m' p) ->
    ProcessT m b c -> ProcessT m' b c
fit f pa =
    lmap Identity $
    fitW runIdentity (\ar (Identity x) -> f (ar x)) pa

-- |Experimental: more general fit.
--
-- Should w be a comonad?
fitW :: (Monad m, Monad m', Functor w) =>
    (forall p. w p -> p) ->
    (forall p q. (p -> m q) -> w p -> m' q) -> 
    ProcessT m b c -> ProcessT m' (w b) c
fitW extr f pa = evolve $ Evolution $ F.F $ \pr0 fr0 ->
    let fr pstep = fr0 $ EvoF (suspend pstep . extr) $ \i ->
            case prepare pstep (extr i)
              of
                Aw fnext -> Aw $ \i2 -> fnext (extr i2)
                Yd x next -> Yd x next
                M mnext -> M (f (\_ -> mnext) $ i)
      in
        F.runF (runEvolution $ finishWith pa) pr0 fr

instance
    Monad m => Profunctor (ProcessT m)
  where
    dimap f g p = evolve $ hdimap f g $ finishWith p 
    {-# INLINE dimap #-}



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
    id = arr id
    {-# INLINE id #-}

    g . f = evolve $ composeEvo (finishWith g) f
    {-# INLINE (.) #-}


instance
    Monad m => Arrow (ProcessT m)
  where
    arr f0 = evolve $ Evolution $ F $ \_ fr -> go f0 fr
      where
        go f fr = 
            fr $ EvoF f $ \_ -> Aw $ \x ->
            fr $ EvoF f $ \_ -> Yd (f x) $
            go f fr
    {-# INLINE arr #-}

    first pa = undefined
    {-# INLINE first #-}

    {-
    second pa = undefined
    {-# INLINE second #-}            
    -}

    {-
    pa0 *** pb = evolve $ Evolution $ F.F $ \pr0 fr0 ->
        let
            fr pbStep pa = undefined
          in
            F.runF (runEvolution $ finishWith pb) absurd fr pa0
    {-# INLINE (***) #-}
    -}    

-- rules
{- RULES
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
  -}
{-
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
-}

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

{-
instance
    (MonadIO m, Occasional o) =>
    MonadIO (Evolution i o m)
  where
    {-# INLINE liftIO #-}
    liftIO ma = lift $ liftIO ma
-}


type PlanT i o = Evolution (Event i) (Event o)

type Plan i o a = forall m. Monad m => PlanT i o m a

{-
"ProcessT: packProc/return"
    forall p. packProc (return p) = p
 -}

 {-
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
-}

class
    MonadAwait m a | m -> a
  where
    await :: m a

instance
    (Monad m, Occasional o) =>
    MonadAwait (Evolution (Event a) o m) a
  where
    {-# INLINE await #-}
    await = Evolution $ F.F go
      where
        go pr fr = fr $ EvoF (const noEvent) $ \_ -> Aw $ \case
            Event x -> pr x
            NoEvent -> go pr fr
            End -> F.runF (runEvolution $ finishWith stopped) pr fr


class
    MonadYield m a | m -> a
  where
    yield :: a -> m ()

instance
    Monad m => MonadYield (Evolution i (Event a) m) a
  where
    {-# INLINE yield #-}
    yield x =Evolution $ F.liftF $ EvoF (const noEvent) $ \_ -> Yd (Event x) ()
          

class
    MonadStop m
  where
    stop :: m a

instance
    (Monad m, Occasional o) =>
    MonadStop (Evolution i o m)
  where
    {-# INLINE stop #-}
    stop = finishWith stopped


catchP:: (Monad m, Occasional' o) =>
    Evolution i o m a -> Evolution i o m a -> Evolution i o m a
catchP p recover = Evolution $ F.F $ \pr0 fr0 ->
    let fr pstep = fr0 $ EvoF (suspend pstep) $ \i ->
            case prepare pstep i
              of
                Aw fnext -> Aw fnext
                Yd (collapse -> End) _ ->
                    M $ return $ F.runF (runEvolution recover) pr0 fr0
                Yd x next -> Yd x next
                M mnext -> M mnext
      in
        F.runF (runEvolution p) pr0 fr

{-
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
-}

{-
{- INLINE awaitProc -}
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

{- INLINE yieldProc -}
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
-}
{-# INLINE stopped #-}
stopped ::
    (Monad m, Occasional o) =>
    ProcessT m i o
stopped = arr $ const end

{-# INLINE constructT #-}
constructT ::
    (Monad m) =>
    PlanT i o m r ->
    ProcessT m (Event i) (Event o)
constructT = evolve . (>> stop)

{-# INLINE realizePlan #-}
realizePlan ::
    Monad m =>
    PlanT i o m a ->
    Evolution (Event i) (Event o) m a
realizePlan = id

{-# INLINE repeatedlyT #-}
repeatedlyT ::
    Monad m =>
    PlanT i o m r ->
    ProcessT m (Event i) (Event o)
repeatedlyT = evolve . forever


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
switchAfter :: Monad m => ProcessT m i (o, Event t) -> Evolution i o m t
switchAfter p0 = Evolution $ F $ \pr0 fr0 ->
    let
        fr p = fr0 $ EvoF (fst . suspend p) $ \i ->
            case (prepare p i)
              of
                Yd (_, Event t) _ -> M (return $ pr0 t)
                Yd (x, _) next -> Yd x next
                Aw fnext -> Aw fnext
                M mnext -> M mnext
      in
        F.runF (runEvolution $ finishWith p0) absurd fr


dSwitchAfter :: Monad m => ProcessT m i (o, Event t) -> Evolution i o m t
dSwitchAfter p0 = Evolution $ F $ \pr0 fr0 ->
    let
        fr p = fr0 $ EvoF (fst . suspend p) $ \i ->
            case (prepare p i)
              of
                Yd (x, Event t) _ -> Yd x (pr0 t)
                Yd (x, _) next -> Yd x next
                Aw fnext -> Aw fnext
                M mnext -> M mnext
      in
        F.runF (runEvolution $ finishWith p0) absurd fr

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
switch sf k = evolve $
  do
    x <- switchAfter sf
    finishWith $ k x


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
dSwitch sf k =  evolve $
  do
    x <- dSwitchAfter sf
    finishWith $ k x

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
rSwitch = evolve . go
  where
    go p = switchAfter (p *** Cat.id) >>= go


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

drSwitch = evolve . go
  where
    go p = dSwitchAfter (p *** Cat.id) >>= go


kSwitch ::
    Monad m =>
    ProcessT m b c ->
    ProcessT m (b, c) (Event t) ->
    (ProcessT m b c -> t -> ProcessT m b c) ->
    ProcessT m b c
kSwitch sf test = undefined


dkSwitch ::
    Monad m =>
    ProcessT m b c ->
    ProcessT m (b, c) (Event t) ->
    (ProcessT m b c -> t -> ProcessT m b c) ->
    ProcessT m b c
dkSwitch sf test = undefined

{-
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
-}

gSwitch ::
    Monad m =>
    ProcessT m b (p, r) ->
    ProcessT m p q ->
    ProcessT m (q, r) (c, Event t) ->
    (ProcessT m p q -> t -> ProcessT m b c) ->
    ProcessT m b c
gSwitch pre sf post = undefined

dgSwitch ::
    Monad m =>
    ProcessT m b (p, r) ->
    ProcessT m p q ->
    ProcessT m (q, r) (c, Event t) ->
    (ProcessT m p q -> t -> ProcessT m b c) ->
    ProcessT m b c
dgSwitch pre sf post = undefined

broadcast ::
    Functor col =>
    b -> col sf -> col (b, sf)
broadcast x sfs = fmap (\sf -> (x, sf)) sfs

par ::
    (Monad m, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessT m ext c) ->
    ProcessT m b (col c)
par r sfs = undefined

parB ::
    (Monad m, Tv.Traversable col) =>
    col (ProcessT m b c) ->
    ProcessT m b (col c)
parB = par broadcast

{-
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
-}

pSwitch ::
    (Monad m, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessT m ext c) ->
    ProcessT m (b, col c) (Event mng) ->
    (col (ProcessT m ext c) -> mng -> ProcessT m b (col c)) ->
    ProcessT m b (col c)
pSwitch r sfs test = undefined

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
dpSwitch r sfs test = undefined

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
rpSwitch r sfs = undefined

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
drpSwitch r sfs = undefined

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
    go = undefined

    fork = undefined -- repeatedly $ await >>= Fd.mapM_ yield

    nullFd = undefined -- getAll . Fd.foldMap (\_ -> All False)


--
-- Running
--
{-
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
-}

-- | Run a machine.
runT ::
    (Monad m, Fd.Foldable f) =>
    (c -> m ()) ->
    ProcessT m (Event b) (Event c) ->
    f b -> m ()
runT = undefined
{-
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
-}

type Builder b = FT.F ((,) b)

putB :: b -> Builder b ()
putB x = FT.liftF (x, ())

bToList :: Builder b a -> [b]
bToList x = build $ \cons nil -> FT.runF x (const nil) (uncurry cons)

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

{-
lftRM :: (Monad m, Monad m') =>
    (forall p. m p -> m' p) ->
    RM i o m a ->
    StateT (RunInfo i o m) m' a
lftRM lft' st = StateT $ \s -> lft' $ runStateT st s
-}

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
stepRun _ _ _ _ = undefined
{-
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
-}

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
stepYield _ _ _ _ = undefined
{-
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
-}
