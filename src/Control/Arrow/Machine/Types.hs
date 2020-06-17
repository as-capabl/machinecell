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
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE PatternSynonyms #-}
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

        -- * Evolutions
        switchAfter,
        dSwitchAfter,
        kSwitchAfter,
        dkSwitchAfter,
        gSwitchAfter,
        dgSwitchAfter,

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

        -- * For additional instances
        chooseProcessT,
        loopProcessT,

        -- * Primitive machines - unsafe
        unsafeExhaust,
      )
where

import qualified Control.Category as Cat
import Data.Profunctor (Profunctor, dimap, rmap, lmap)
import Data.Functor.Apply
import Data.Void
import Data.Maybe (fromMaybe)
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Skeleton
import Control.Applicative
import Control.Comonad
import qualified Data.Foldable as Fd
import Data.Traversable as Tv
import Data.Semigroup (Semigroup ((<>)))
import Control.Monad.Trans.Free.Church (FT ())
import qualified Control.Monad.Trans.Free.Church as FT
import Control.Monad.Free.Church (F ())
import qualified Control.Monad.Free.Church as F
import Control.Comonad.Cofree (Cofree (..))
import qualified Control.Comonad.Cofree as F
import Control.Monad.Free
import qualified Control.Monad.Free as Free
import GHC.Exts (build)
import Unsafe.Coerce

import Control.Arrow.Machine.Internal.Event

data EvoV_ i o m a =
    Aw (i -> a) |
    Yd o a |
    M (m a)
  deriving Functor

type EvoV = EvoV_
type EvoV_UG = EvoV_

data EvoF' evoV i o m a = EvoF
  {
    suspend :: i -> o,
    prepare :: i -> evoV i o (m :: * -> *) a  
  }
  deriving Functor

-- type role EvoF' representational representational representational representational representational

type EvoF = EvoF' EvoV
type EvoF_UG = EvoF' EvoV_UG

data UGStack i a = UGStack
  {
    accept :: a,
    unGet :: i -> a
  }
    deriving Functor

instance Comonad (UGStack i)
  where
    extract = accept
    duplicate wa = UGStack wa (ugPure . unGet wa)

ugPure :: a -> UGStack i a
ugPure x = UGStack x (const x)

makeUG :: (Maybe i -> a) -> UGStack i a
makeUG f = UGStack (f Nothing) (f . Just)

elimUG :: UGStack i a -> Maybe i -> a
elimUG s Nothing = extract s
elimUG s (Just x) = unGet s x

ugSequence :: Functor f => f (UGStack i a) -> UGStack i (f a)
ugSequence fux = makeUG $ \mu -> (elimUG `flip` mu) <$> fux

newtype Evolution i o m r = Evolution
  {
    runEvolution :: FT (EvoF i o m) (UGStack i) r
  }

{-# RULES
"runEvo/makeEvo"
        forall
            (s :: forall r. (Void -> r) -> (EvoF i o m r -> r) -> r)
            (fr :: forall r. EvoF i o m r -> r).
                runEvo (makeEvo s) fr = s absurd fr
"makeEvo/runEvo"
        forall e.
            makeEvo (\_ fr -> runEvo e fr) = e
#-}

{-# INLINE [0] runEvo #-}
runEvo :: Monad m => Evolution i o m Void -> (EvoF i o m r -> r) -> r
runEvo evo fr0 = extract (FT.runFT (runEvolution evo) absurd (\xr fx -> fr0 <$> ugSequence (xr <$> fx)))

{-# INLINE [0] makeEvo #-}
makeEvo :: Monad m => (forall r. (a -> r) -> (EvoF i o m r -> r) -> r) -> Evolution i o m a
makeEvo f = Evolution $ FT.FT $ \pr fr0 -> f pr (fr fr0)
  where
    fr fr0 evoF = makeUG $ \mu -> extract $ fr0 id $ EvoF (suspend evoF) $ \i -> case prepare evoF i
      of
        Aw f -> case mu of
            Nothing -> Aw f
            Just u -> M . return $ f u
        Yd y r -> Yd y r
        M mr -> M $ extend (elimUG `flip` mu) <$> mr

aw_ :: (i -> o) -> (i -> a) -> Evolution i o m a
aw_ sus f = Evolution $ FT.FT $ \pr fr ->
    let
        r = fr pr (EvoF sus $ \_ -> Aw f)
      in
        UGStack (extract r) (extract . pr . f)

yd_ :: (i -> o) -> o -> Evolution i o m ()
yd_ sus x = Evolution $ FT.FT $ \pr fr ->
    ugPure . extract $ fr pr (EvoF sus $ \_ -> Yd x ())

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
    lift ma = Evolution $ F.wrap $ EvoF (const noEvent) (\_ -> M (return <$> ma))

-- | The stream transducer arrow.
--
-- To construct `ProcessT` instances, use `Control.Arrow.Machine.Plan.Plan`,
-- `arr`, functions declared in `Control.Arrow.Machine.Utils`,
-- or arrow combinations of them.
--
-- See an introduction at "Control.Arrow.Machine" documentation.
newtype ProcessT m i o = ProcessT { runProcessT :: Skeleton (EvoF i o m) Void }

unwrapP :: Functor m => ProcessT m i o -> EvoF i o m (ProcessT m i o)
unwrapP pa = case debone (runProcessT pa)
  of
    Return v -> absurd v
    evoF :>>= cnt -> ProcessT <$> (cnt <$> evoF)

{-# INLINE [1] compositeProc #-}
compositeProc f g = evolve $ composeEvo (finishWith g) f

composeEvo :: forall a b c m. Monad m => Evolution b c m Void -> ProcessT m a b -> Evolution a c m Void
composeEvo q0 p0 = makeEvo $ \_ fr0 ->
    let
        goF p evoF = EvoF (suspend evoF . suspend (unwrapP p)) $ \i ->
            let
                susX = suspend (unwrapP p) i
                vQ = prepare evoF susX
                vP = prepare (unwrapP p) i
              in
                case (vP, vQ)
                  of
                    (_, M mq') -> M $ ($p) <$> mq'
                    (_, Yd o q') -> Yd o (q' p)
                    (M mp', Aw _) -> M $ go evoF <$> mp'
                    (Yd z p', Aw g) -> M $ return $ g z p'
                    (Aw f, Aw _) -> Aw $ go evoF . f
        go evoF p = fr0 (goF p evoF)
      in
        runEvo q0 go p0

{- unsafeCoerce $
    let
        go :: (ProcessT m a b, Skeleton (EvoF b c m) Void) -> Skeleton (EvoF a c m) Void
        go pq = boned $ goF pq :>>= go

        goF (_, debone -> Return v) = absurd v
        goF (p, q@(debone -> evoF :>>= cnt)) = EvoF (suspend evoF . suspend (unwrapP p)) $ \i ->
            let
                susX = suspend (unwrapP p) i
                vQ = prepare evoF susX
                vP = prepare (unwrapP p) i
              in
                EV $ case (vP, vQ)
                  of
                    (_, M mq') -> M $ (p,) . cnt <$> mq'
                    (_, Yd o q') -> Yd o (p, cnt q')
                    (M mp', Aw _) -> M $ (,q) <$> mp'
                    (Yd z p', Aw g) -> prepare (goF (p', cnt $ g z)) i
                    (Aw f, Aw _) -> Aw $ (,q) . f 
      in
        go (p0, resolveUG $ runEvolution q0)
-}

{-# RULES
    "evolve/finishWith" forall p. evolve (finishWith p) = p
#-}
{-# INLINE[0] evolve #-}
evolve :: Monad m => Evolution i o m Void -> ProcessT m i o
evolve evo = ProcessT $ runEvo evo (\fx -> boned (fx :>>= id))

{-# INLINE[0] finishWith #-}
finishWith :: Monad m => ProcessT m i o -> Evolution i o m a
finishWith (ProcessT pa0) = makeEvo $ \pr fr -> deboneBy (go pr fr) pa0
  where
    go pr _ (Return x) = pr (absurd x)
    go pr fr (fx :>>= xr) = fr (deboneBy (go pr fr) . xr <$> fx)



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
fitW extr f pa = evolve $ makeEvo $ \_ fr0 ->
    let fr pstep = fr0 $ EvoF (suspend pstep . extr) $ \i ->
            case prepare pstep (extr i)
              of
                Aw fnext -> Aw $ \i2 -> fnext (extr i2)
                Yd x next -> Yd x next
                M mnext -> M (f (\_ -> mnext) $ i)
      in
        runEvo (finishWith pa) fr

{-# INLINE [1] dimapProc #-}
dimapProc :: Monad m => (a -> b) -> (c -> d) -> ProcessT m b c -> ProcessT m a d
dimapProc f g p = evolve $ makeEvo $ \_ fr0 ->
    let
        fr evoF = fr0 $ EvoF (dimap f g $ suspend evoF) $ \i ->
            case prepare evoF (f i)
              of
                Aw fnext -> Aw $ fnext . f
                Yd x next -> Yd (g x) next
                M mnext -> M mnext
      in
        runEvo (finishWith p) fr

instance
    Monad m => Profunctor (ProcessT m)
  where
    dimap = dimapProc
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
    id = idProc
    {-# INLINE id #-}

    g . f = compositeProc f g
    {-# INLINE (.) #-}

{-# INLINE [1] idProc #-}
idProc :: Monad m => ProcessT m a a
idProc = evolve $ forever $ aw_ id id >>= yd_ id

{-# INLINE [1] arrProc #-}
arrProc :: Monad m => (a -> b) -> ProcessT m a b
arrProc f0 = evolve $ forever $ aw_ f0 f0 >>= yd_ f0

{-# INLINE [1] parProc #-}
parProc :: Monad m =>
    ProcessT m a b -> ProcessT m c d -> ProcessT m (a, c) (b, d)
parProc p0 pa0 = evolve $ makeEvo $ \pr0 fr0 ->
    let
        fr paStep p = fr0 $ EvoF (suspend (unwrapP p) *** suspend paStep) $ \(sus, susA) ->
            case (prepare (unwrapP p) sus, prepare paStep susA)
              of
                (_, M mnext) -> M $ do { next <- mnext; return $ next p }
                (M mp', _) -> M $ do { p' <- mp'; return $ fr paStep p' }
                (Yd y p', Yd ya next) -> Yd (y, ya) $ next p'
                (_, Yd ya next) -> Yd (suspend (unwrapP p) sus, ya) $ next p
                (Yd y p', _) -> Yd (y, suspend paStep susA) $ fr paStep p'
                (Aw fp', Aw fnext) -> Aw (\(x, xa) -> fnext xa (fp' x))
      in
        runEvo (finishWith pa0) fr p0

instance
    Monad m => Arrow (ProcessT m)
  where
    arr = arrProc
    {-# INLINE arr #-}

    {-
    first pa0 = evolve $ Evolution $ F.F $ \pr0 fr0 ->
        let
            fr paStep = fr0 $ EvoF (first $ suspend paStep) $ \(i, d) ->
                case prepare paStep i
                  of
                    Aw fnext -> Aw (fnext . fst) -- Don't ignore snd!
                    Yd x next -> Yd (x, d) next
                    M mnext -> M mnext
          in
            F.runF (runEvolution $ finishWith pa0) pr0 fr
    {-# INLINE first #-}
    -}

    {-
    second pa = undefined
    {-# INLINE second #-}            
    -}

    (***) = parProc
    {-# INLINE (***) #-} 

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
"ProcessT: arr/*"
    forall f g. compositeProc (arrProc f) g = dimapProc f id g
"ProcessT: */arr"
    forall f g. compositeProc f (arrProc g) = dimapProc id g f
"ProcessT: arr***arr"
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
    await = Evolution $ FT.FT $ \pr fr ->
        let
            r = fr doAw (EvoF (const noEvent) $ \_ -> Aw id)
            doAw (Event x) = pr x
            doAw NoEvent = r
            doAw End = FT.runFT (runEvolution stop) pr fr
          in
            UGStack (extract r) (extract . doAw)

class
    MonadYield m a | m -> a
  where
    yield :: a -> m ()

instance
    Monad m => MonadYield (Evolution i (Event a) m) a
  where
    {-# INLINE yield #-}
    yield = yd_ (const NoEvent) . Event

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
catchP p recover = undefined {- Evolution $
    let 
        go (Just (debone -> Return x), _) = return x
        go (Just (debone -> pstep :>>= cnt), lastval) = boned $ goMV pstep lastval :>>= \(x, lv') -> go (cnt <$> x, lv') 
        go (Nothing, _) = runEvolution recover

        goMV pstep lastval = EvoF (suspend pstep) $ \i ->
            case prepare pstep i
              of
                Aw fnext -> Aw $ \x -> (Just $ fnext x, (Just x))
                Yd (collapse -> End) _ -> case lastval
                  of
                    Just x -> UnGet x (Nothing, Nothing)
                    Nothing -> Nop () (Nothing, Nothing)
                Yd x next -> Yd x (Just next, Nothing)
                M mnext -> M $ do { next <- mnext; return (Just next, lastval) }
                UnGet x next -> UnGet x (Just next, Nothing)
                Nop () next -> Nop () (Just next, lastval)
      in
        go (Just $ runEvolution p, Nothing)
-}

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
stopped = evolve $ makeEvo $ \_ fr -> go fr
      where
        go fr =
            fr $ EvoF (const end) $ \_ -> Yd end $ 
            fr $ EvoF (const end) $ \_ -> Aw $ \_ ->
            go fr

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
switchAfter p0 = Evolution $ FT.FT $ \pr fr ->
    let
        go _ (debone -> Return x) = absurd x
        go mleft (debone -> EvoF sus prep :>>= cnt) = fr id $ EvoF (fst . sus) $ \i ->
            case prep i
              of
                Aw f -> M . return $
                    UGStack
                        (extract (fr id $ EvoF (fst . sus) (\_ ->
                            Aw (\x -> go (Just x) (cnt $ f x)))))
                        (extract . go Nothing . cnt . f)
                Yd (_, Event t) _ -> M . return $ case mleft
                  of
                    Just x -> ugPure $ unGet (pr t) x
                    Nothing -> pr t
                Yd (y, _) r -> Yd y $ go Nothing (cnt r)
                M mr -> M (go mleft . cnt <$> mr)
      in
        go Nothing (runProcessT p0)

dSwitchAfter :: Monad m => ProcessT m i (o, Event t) -> Evolution i o m t
dSwitchAfter p0 = Evolution $ FT.FT $ \pr fr ->
    let
        go (debone -> Return x) = absurd x
        go (debone -> EvoF sus prep :>>= cnt) = fr id $ EvoF (fst . sus) $ \i ->
            case prep i
              of
                Aw f -> M . return $
                    UGStack
                        (extract (fr id $ EvoF (fst . sus) $ \_ ->
                            Aw (\x -> go (cnt $ f x))))
                        (extract . go . cnt . f)
                Yd (y, Event t) _ -> Yd y (pr t)
                Yd (y, _) r -> Yd y (go . cnt $ r)
                M mr -> M (go . cnt <$> mr)
      in
        go (runProcessT p0)

{-# INLINE kSwitchAfter #-}
kSwitchAfter ::
    Monad m =>
    ProcessT m (i, o) (Event r) ->
    ProcessT m i o ->
    Evolution i o m (ProcessT m i o, r)
kSwitchAfter test = g1SwitchAfter id (arr snd &&& test)

{-# INLINE dkSwitchAfter #-}
dkSwitchAfter ::
    Monad m =>
    ProcessT m (i, o) (Event r) ->
    ProcessT m i o ->
    Evolution i o m (ProcessT m i o, r)
dkSwitchAfter test = dg1SwitchAfter id (arr snd &&& test)

{-# INLINE g1SwitchAfter #-}
g1SwitchAfter ::
    Monad m =>
    (i -> p) ->
    ProcessT m (i, q) (o, Event t) ->
    ProcessT m p q ->
    Evolution i o m (ProcessT m p q, t)
g1SwitchAfter pre post pf = undefined {- Evolution $ 
    let
        go (_, debone -> Return v, _) = absurd v
        go (p, qskel @ (debone -> q :>>= cnt), lv) = boned $ goMV (p, cnt <$> q, lv) :>>= \case
            Left r -> return r
            Right (p', q', lv') -> go (p', fromMaybe qskel q', lv')

        goMV (p, q, lastval) = EvoF (fst . suspend q . (id &&& suspend (unwrapP p) . pre)) $ \i ->
            let
                susX = suspend (unwrapP p) $ pre i
                vQ = prepare q (i, susX)
                vP = prepare (unwrapP p) $ pre i
              in
                case (vP, vQ)
                  of
                    (_, M mq') -> M $ mq' >>= \q' -> return $ Right (p, Just q', lastval)
                    (_, Yd (o, Event t) q') -> goRecover lastval $ Left (p, t)
                    (_, Yd (o, _) q') -> Yd o $ Right (p, Just q', Nothing)
                    (M mp', Aw _) -> M $ mp' >>= \p' -> return $ Right (p', Nothing, lastval)
                    (Yd z p', Aw g) -> M $ return $ Right $ (p', Just $ g (maybe i id lastval, z), lastval)
                    (Aw f, Aw _) -> Aw $ \i2 -> Right $ (f $ pre i2, Nothing, Just i2)
      in
        go (pf, runProcessT post, Nothing)
  where
    goRecover (Just x) = UnGet x
    goRecover Nothing = Nop ()
-}

{-# INLINE dg1SwitchAfter #-}
dg1SwitchAfter ::
    Monad m =>
    (i -> p) ->
    ProcessT m (i, q) (o, Event t) ->
    ProcessT m p q ->
    Evolution i o m (ProcessT m p q, t)
dg1SwitchAfter pre post pf = makeEvo $ \pr0 fr0 -> 
    let
        fr q lastval p = fr0 $ EvoF (fst . suspend q . (id &&& suspend (unwrapP p) . pre)) $ \i ->
            let
                susX = suspend (unwrapP p) $ pre i
                vQ = prepare q (i, susX)
                vP = prepare (unwrapP p) $ pre i
              in
                case (vP, vQ)
                  of
                    (_, M mq') -> M (mq' >>= \q' -> return $ q' lastval p)
                    (_, Yd (o, Event t) q') -> Yd o $ pr0 (p, t)
                    (_, Yd (o, _) q') -> Yd o $ q' Nothing p
                    (M mp', Aw _) -> M (fr q lastval <$> mp')
                    (Yd z p', Aw g) -> M . return $ g (maybe i id lastval, z) lastval p'
                    (Aw f, Aw _) -> Aw $ (\i2 -> fr q (Just i2) (f $ pre i2)) 
      in
        runEvo (finishWith post) fr Nothing pf

{-# INLINE gSwitchAfter #-}
gSwitchAfter ::
    Monad m =>
    ProcessT m i (p, r) ->
    ProcessT m (q, r) (o, Event t) ->
    ProcessT m p q ->
    Evolution i o m (ProcessT m p q, t)
gSwitchAfter pre post pf = switchAfter $ pre >>> evolve pfpost
  where
    pfpost =
      do
        (pf', (lastval, ret)) <- g1SwitchAfter fst post' pf
        finishWith $ pure lastval &&& (stopped >>> construct (yield (pf', ret)))
    
    post' = proc ((_, r), q) ->
      do
        (o, evt) <- post -< (q, r)
        returnA -< ((o, noEvent), (o,) <$> evt)

{-# INLINE dgSwitchAfter #-}
dgSwitchAfter ::
    Monad m =>
    ProcessT m i (p, r) ->
    ProcessT m (q, r) (o, Event t) ->
    ProcessT m p q ->
    Evolution i o m (ProcessT m p q, t)
dgSwitchAfter pre post pf = switchAfter $ pre >>> evolve pfpost
  where
    pfpost =
      do
        (pf', (lastval, ret)) <- dg1SwitchAfter fst post' pf
        finishWith $ pure lastval &&& (stopped >>> construct (yield (pf', ret)))
  
    post' = proc ((_, r), q) ->
      do
        (o, evt) <- post -< (q, r)
        returnA -< ((o, noEvent), (o,) <$> evt)

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
rSwitch p0 = evolve $ switchAfter (p0 *** Cat.id) >>= go
  where
    go p = switchAfter (p *** evolve (await >> finishWith Cat.id)) >>= go


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
kSwitch sf test next = evolve $
  do
    (sf', t) <- kSwitchAfter test sf
    finishWith $ next sf' t


dkSwitch ::
    Monad m =>
    ProcessT m b c ->
    ProcessT m (b, c) (Event t) ->
    (ProcessT m b c -> t -> ProcessT m b c) ->
    ProcessT m b c
dkSwitch sf test next = evolve $
  do
    (sf', t) <- dkSwitchAfter test sf
    finishWith $ next sf' t

gSwitch ::
    Monad m =>
    ProcessT m b (p, r) ->
    ProcessT m p q ->
    ProcessT m (q, r) (c, Event t) ->
    (ProcessT m p q -> t -> ProcessT m b c) ->
    ProcessT m b c
gSwitch pre sf post next = evolve $
  do
    (sf', t) <- gSwitchAfter pre post sf
    finishWith $ next sf' t

dgSwitch ::
    Monad m =>
    ProcessT m b (p, r) ->
    ProcessT m p q ->
    ProcessT m (q, r) (c, Event t) ->
    (ProcessT m p q -> t -> ProcessT m b c) ->
    ProcessT m b c
dgSwitch pre sf post next = evolve $
  do
    (sf', t) <- dgSwitchAfter pre post sf
    finishWith $ next sf' t

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
unsafeExhaust p = evolve $ makeEvo $ \_ fr0 ->
    let doCycle = fr0 $ EvoF (const NoEvent) $ \i ->
            M (Fd.foldr yd nextCycle <$> p i)
        yd x next = fr0 $ EvoF (const NoEvent) $ \_ ->
            Yd (Event x) next
        nextCycle = fr0 $ EvoF (const NoEvent) $ \_ ->
            Aw (\_ -> doCycle)
      in
        doCycle


-- | Run a machine.
runT ::
    (Monad m, Fd.Foldable f) =>
    (c -> m ()) ->
    ProcessT m (Event b) (Event c) ->
    f b -> m ()
runT outpre pa0 l = Fd.foldr frF (term False) l (runProcessT pa0)
  where
    frF _ _ (debone -> Return v) = absurd v
    frF x next (debone -> evoF :>>= cnt) =
        case
            prepare evoF NoEvent
          of
            Yd (Event y) p' -> outpre y >> frF x next (cnt p')
            Yd NoEvent p' -> frF x next (cnt p')
            Yd End p' -> term False (cnt p')
            M mp' -> cnt <$> mp' >>= frF x next 
            Aw fp' -> next (cnt $ fp' (Event x))

    term _ (debone -> Return v) = absurd v
    term b (debone -> evoF :>>= cnt) =
        case
            prepare evoF NoEvent
          of
            Yd (Event y) p' -> outpre y >> term False (cnt p')
            Yd _ p' -> term b (cnt p') 
            M mp' -> cnt <$> mp' >>= term b
            Aw fp' -> if b
                then return ()
                else term True (cnt $ fp' End)


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
stepRun lft yd stp pa0 x = go pa0 (Just x)
  where
    go pa mx = case prepare (unwrapP pa) noEvent
      of
        Aw f -> case mx
          of
            Just i -> go (f $ Event i) Nothing
            Nothing -> return pa
        Yd (Event y) pa' -> yd y >> go pa' mx
        Yd NoEvent pa' -> go pa' mx
        Yd End pa' -> lft (runT_ pa' []) >> stp mx >> return stopped
        M mpa' -> do { pa' <- lft mpa'; go pa' mx }

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
stepYield lft aw stp pa0 =  go pa0 False
  where
    go pa done = case prepare (unwrapP pa) noEvent
      of
        Aw f -> case done
          of 
            False -> aw >>= \i -> go (f $ Event i) True
            True -> return (Nothing, pa)
        Yd (Event y) pa' -> return (Just y, pa')
        Yd NoEvent pa' -> go pa' done
        Yd End pa' -> lft (runT_ pa' []) >> stp >> return (Nothing, stopped)
        M mpa' -> do { pa' <- lft mpa'; go pa' done }

chooseProcessT ::
    Monad m =>
    ProcessT m a1 b1 ->
    ProcessT m a2 b2 ->
    ProcessT m (Either a1 a2) (Either b1 b2)
chooseProcessT p0 q0 = evolve $ makeEvo $ \_ fr0 ->
    let
        go p@(unwrapP -> evoP) q@(unwrapP -> evoQ) = fr0 $ EvoF (suspend evoP +++ suspend evoQ) $ \case
            Left i -> goV (\case {Left i2 -> i2; _ -> i}) Left (prepare evoP i) $ \p' -> go p' q
            Right i -> goV (\case {Right i2 -> i2; _ -> i}) Right (prepare evoQ i) $ \q' -> go p q'
        goV awPre _ (Aw fnext) r = Aw (r . fnext . awPre)
        goV _ ydPre (Yd x next) r = Yd (ydPre x) (r next)
        goV _ _ (M mnext) r = M $ r <$> mnext
      in
        go p0 q0


loopProcessT ::
    Monad m =>
    ProcessT m (a, d) (b, d) ->
    ProcessT m a b
loopProcessT p = evolve $ makeEvo $ \_ fr0 ->
    let
        fr evoF = fr0 $ let
            sus = loop $ (\(o, d) -> ((o, d), d)) . suspend evoF
            susO = fst . sus
            susD = snd . sus
          in
            EvoF susO $ \i -> case prepare evoF (i, susD i)
              of
                Aw fnext -> Aw $ \i' -> fnext (i', susD i) -- susD i' ?
                Yd (o, _) next -> Yd o next
                M mnext -> M mnext
      in
        runEvo (finishWith p) fr
