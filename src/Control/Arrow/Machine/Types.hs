{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module
    Control.Arrow.Machine.Types
      (
        -- * Basic types
        ProcessA(),

        Occasional' (..),
        Occasional (..),
        Event (),
        condEvent,
        filterEvent,
        evMap,
        
        -- * Plan monads
        PlanT,
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
        --pSwitch,
        --pSwitchB,
        --rpSwitch,
        --rpSwitchB,
        --par,
        --parB,

        
        -- * Primitive machines - other safe primitives
        fit,
        
        -- * Primitive machines - unsafe
        unsafeExhaust,
      )
where

import qualified Control.Category as Cat
import Data.Profunctor (Profunctor, dimap, rmap)
import Control.Arrow.Operations (ArrowReader(..))
import Control.Arrow.Transformer.Reader (ArrowAddReader(..))
import Control.Arrow
import Control.Monad hiding (join)
import Control.Monad.Trans
import Control.Monad.State hiding (join)
import Control.Monad.Writer hiding ((<>), join)
import Control.Applicative hiding (pure)
import qualified Control.Applicative as Ap
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

step = id

-- | The stream transducer arrow.
--
-- To construct `ProcessA` instances, use `Control.Arrow.Machine.Plan.Plan`,
-- `arr`, functions declared in `Control.Arrow.Machine.Utils`,
-- or arrow combinations of them.
data ProcessA a b c = ProcessA {
    feed :: a b (c, ProcessA a b c),
    sweep :: a b (Maybe c, ProcessA a b c),
    suspend :: b -> c
  }



fit :: (Arrow a, Arrow a') => 
       (forall p q. a p q -> a' p q) -> 
       ProcessA a b c -> ProcessA a' b c
fit f (ProcessA {..})  = ProcessA
  {
    feed = f (feed >>> second (arr $ fit f)),
    sweep = f (sweep >>> second (arr $ fit f)),
    suspend = suspend
  }



instance
    Arrow a => Profunctor (ProcessA a)
  where
    dimap f g pa = dimapProc f g (step pa)
    {-# INLINE dimap #-}

dimapProc :: Arrow a => 
             (b->c)->(d->e)->
             ProcType a c d -> ProcType a b e
dimapProc f g (ProcessA {..}) = ProcessA 
  {
    feed = arr f >>> feed >>> (arr g *** arr (dimapProc f g)) ,
    sweep = arr f >>> sweep >>> (arr (fmap g) *** arr (dimapProc f g)),
    suspend = dimap f g suspend
  }

{-# NOINLINE dimapProc #-}


instance
    Arrow a => Functor (ProcessA a i)
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
    id = idProc
    {-# INLINE id #-}
    g . f = compositeProc (step f) (step g)
    {-# INLINE (.) #-}


instance 
    ArrowApply a => Arrow (ProcessA a)
  where
    arr = arrProc
    {-# INLINE arr #-}

    first pa = parProc (step pa) idProc
    {-# INLINE first #-}

    second pa = parProc idProc (step pa)
    {-# INLINE second #-}

    pa *** pb = parProc (step pa) (step pb)
    {-# INLINE (***) #-}


parProc :: ArrowApply a =>
    ProcType a b c ->
    ProcType a d e ->
    ProcType a (b, d) (c, e)
parProc f g = ProcessA {
    feed = proc (x1, x2) ->
      do
        (y1, f') <- feed f -< x1
        (y2, g') <- feed g -< x2
        returnA -< ((y1, y2), parProc f' g'),
    sweep = proc (x1, x2) ->
      do
        (my1, f') <- sweep f -< x1
        (my2, g') <- sweep g -< x2
        let y1 = fromMaybe (suspend f x1) my1
            y2 = fromMaybe (suspend g x2) my2
            r = if (isNothing my1 && isNothing my2) then Nothing else Just (y1, y2)
        returnA -< (r, parProc f' g'),
    suspend = suspend f *** suspend g
  }
{-# NOINLINE parProc #-}

idProc :: ArrowApply a => ProcType a b b
idProc = ProcessA {
    feed = arr $ \x -> (x, idProc),
    sweep = arr $ \_ -> (Nothing, idProc),
    suspend = id
  }
{-# NOINLINE idProc #-}

arrProc :: ArrowApply a => (b->c) -> ProcType a b c
arrProc f =  ProcessA {
    feed = arr $ \x -> (f x, arrProc f),
    sweep = arr $ \_ -> (Nothing, arrProc f),
    suspend = f
  }
{-# NOINLINE arrProc #-}

-- |Composition is proceeded by the backtracking strategy.
compositeProc :: ArrowApply a => 
              ProcType a b d -> ProcType a d c -> ProcType a b c
compositeProc f g = ProcessA {
    feed = proc x ->
      do
        (y, f') <- feed f -< x
        (z, g') <- feed g -< y
        returnA -< (z, compositeProc f' g'),
    sweep = proc x ->
      do
        (mz, g') <- sweep g -< suspend f x
        (case mz
          of
            Just z -> arr $ const (Just z, compositeProc f g')
            Nothing -> btrk f g')
                -<< x,
    suspend = suspend f >>> suspend g
  }
  where
    btrk f g = proc x ->
      do
        (my, f') <- sweep f -< x
        (mz, g') <-
            (case my
              of
                Just y -> proc () ->
                  do
                    (z, g') <- feed g -< y
                    returnA -< (Just z, g')
                Nothing -> proc () ->
                  do
                    returnA -< (Nothing, g))
                -<< ()
        returnA -< (mz, compositeProc f' g')

{-# NOINLINE compositeProc #-}

-- rules
{-# RULES
"ProcessA: id/*"
    forall g. compositeProc idProc g = g
"ProcessA: */id"
    forall f. compositeProc f idProc = f

"ProcessA: concat/concat" 
    forall f g h. compositeProc (compositeProc f g) h = compositeProc f (compositeProc g h)

"ProcessA: dimap/dimap"
    forall f g h i j. dimapProc f j (dimapProc g i h)  = dimapProc (g . f) (j . i) h
"ProcessA: dimap/arr"
    forall f g h. dimapProc f h (arrProc g) = arrProc (h . g . f)

"ProcessA: arr***/par"
    forall f1 f2 g1 g2 h. compositeProc (parProc f1 (arrProc f2)) (compositeProc (parProc g1 g2) h) =
        compositeProc (parProc (compositeProc f1 g1) (dimapProc f2 id g2)) h
"ProcessA: arr***/par-2"
    forall f1 f2 g1 g2. compositeProc (parProc f1 (arrProc f2)) (parProc g1 g2) =
        parProc (compositeProc f1 g1) (dimapProc f2 id g2)
"ProcessA: par/***arr"
    forall f1 f2 g1 g2 h. compositeProc (parProc f1 f2) (compositeProc (parProc (arrProc g1) g2) h) =
        compositeProc (parProc (dimapProc id g1 f1) (compositeProc f2 g2)) h
"ProcessA: par/***arr-2"
    forall f1 f2 g1 g2. compositeProc (parProc f1 f2) (parProc (arrProc g1) g2) =
        parProc (dimapProc id g1 f1) (compositeProc f2 g2)

"ProcessA: first/par"
    forall f1 g1 g2 h. compositeProc (parProc f1 idProc) (compositeProc (parProc g1 g2) h) =
        compositeProc (parProc (compositeProc f1 g1) g2) h
"ProcessA: first/par-2"
    forall f1 g1 g2. compositeProc (parProc f1 idProc) (parProc g1 g2) =
        parProc (compositeProc f1 g1) g2
"ProcessA: par/second"
    forall f1 f2 g2 h. compositeProc (parProc f1 f2) (compositeProc (parProc idProc g2) h) =
        compositeProc (parProc f1 (compositeProc f2 g2)) h
"ProcessA: par/second-2"
    forall f1 f2 g2. compositeProc (parProc f1 f2) (parProc idProc g2) =
        parProc f1 (compositeProc f2 g2)

"ProcessA: arr/arr"
    forall f g h. compositeProc (arrProc f) (compositeProc (arrProc g) h) =
        compositeProc (arrProc (g . f)) h
"ProcessA: arr/arr-2"
    forall f g. compositeProc (arrProc f) (arrProc g) = arrProc (g . f)
"ProcessA: arr/*" [1]
    forall f g. compositeProc (arrProc f) g = dimapProc f id g
"ProcessA: */arr" [1]
    forall f g. compositeProc f (arrProc g) = dimapProc id g f
"ProcessA: arr***arr" [0]
    forall f g. parProc (arrProc f) (arrProc g) = arrProc (f *** g)
  #-}


instance
    ArrowApply a => ArrowChoice (ProcessA a)
  where
    left pa0@ProcessA{..} = ProcessA {
        feed = proc eth -> feed' eth -<< (),
        sweep = proc eth -> sweep' eth -<< (),
        suspend = left suspend
      }
      where
        feed' (Left x) = proc () ->
          do
            (y, pa) <- feed -< x
            returnA -< (Left y, left pa)
        feed' (Right d) = proc () ->
            returnA -< (Right d, left pa0)

        sweep' (Left x) = proc () ->
          do
            (my, pa) <- sweep -< x
            returnA -< (Left <$> my, left pa)
        sweep' (Right d) = proc () ->
            returnA -< (Nothing, left pa0)



instance
    ArrowApply a => ArrowLoop (ProcessA a)
  where
    loop (ProcessA {..}) = ProcessA {
        feed = (Cat.id &&& arr loopSusD) >>> feed >>> arr (\(x, pa') -> (fst x, loop pa')),
        sweep = (Cat.id &&& arr loopSusD) >>> sweep >>> arr (\(mx, pa') -> (fmap fst mx, loop pa')),
        suspend = loop suspend
      }
      where
        loopSusD = loop (suspend >>> \(_, d) -> (d, d))
    
data Event a = Event a | NoEvent | End deriving (Eq, Show)


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
-- For composite structure, `collapse` can be defined as monoidal sum of all member occasionals.
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



-- TODO: テスト
condEvent :: Bool -> Event a -> Event a
condEvent _ End = End
condEvent True ev = ev
condEvent False _ = NoEvent

-- TODO: テスト
filterEvent :: (a -> Bool) -> Event a -> Event a
filterEvent cond ev@(Event x) = condEvent (cond x) ev
filterEvent _ ev = ev

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
    ed <- repeatedly $ await `catchP` yield () -< collapse x
    rSwitch (arr $ const noEvent) -< ((), stopped <$ ed)



data PlanF i o a where
  AwaitPF :: (i->a) -> a -> PlanF i o a
  YieldPF :: o -> a -> PlanF i o a
  StopPF :: PlanF i o a

instance (Functor (PlanF i o)) where
  fmap g (AwaitPF f ff) = AwaitPF (g . f) (g ff)
  fmap g (YieldPF x r) = YieldPF x (g r)
  fmap _ StopPF = StopPF


type PlanT i o m a = F.FT (PlanF i o) m a
type Plan i o a = forall m. Monad m => PlanT i o m a


yield :: o -> Plan i o ()
yield x = F.liftF $ YieldPF x ()

await :: Plan i o i
await = F.FT $ \pure free -> free id (AwaitPF pure (free pure StopPF))

stop :: Plan i o a
stop = F.liftF $ StopPF


catchP:: Monad m =>
    PlanT i o m a -> PlanT i o m a -> PlanT i o m a

catchP pl cont0 = 
    F.FT $ \pure free ->
        F.runFT
            pl
            (pure' pure)
            (free' cont0 pure free)
  where
    pure' pure = pure

    free' ::
        Monad m =>
        PlanT i o m a ->
        (a -> m r) ->
        (forall x. (x -> m r) -> PlanF i o x -> m r) ->
        (y -> m r) ->
        (PlanF i o y) ->
        m r
    free' cont pure free _ StopPF =
        F.runFT cont pure free
    free' cont pure free r (AwaitPF f ff) =
        free
            (either (\_ -> F.runFT cont pure free) r)
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
constructT' fit0 pl0 = prependProc $ F.runFT pl0 pure free
  where
    fit' :: (b -> m c) -> a b c
    fit' fmy = proc x -> fit0 (fmy x) -<< ()

    prependProc ::
        m (Event o, ProcessA a (Event i) (Event o)) ->
        ProcessA a (Event i) (Event o)
    prependProc mr = ProcessA {
        feed = proc ex -> do { r <- fit0 mr -< (); prependFeed r -<< ex} ,
        sweep = proc ex -> do { r <- fit0 mr -< (); prependSweep r -<< ex},
        suspend = const NoEvent
      }

    prependFeed (Event x, pa) = arr $ const (Event x, pa)
    prependFeed (NoEvent, pa) = feed pa
    prependFeed (End, pa) = arr $ const (End, stopped)
  
    prependSweep (Event x, pa) = arr $ const (Just (Event x), pa)
    prependSweep (NoEvent, pa) = sweep pa
    prependSweep (End, pa) = arr $ const (Just End, stopped)
  
    pure _ = return (End, stopped)

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
        feed = fit' fma,
        sweep = fit' fma >>> first eToM,
        suspend = const NoEvent
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
switchCore ::
    (Arrow cat, Arrow a2, Arrow cat1, Occasional t3) =>
    (t4
     -> a2 (t5, (t6, c1)) c1
     -> (t -> t1 -> cat a (t2, t3))
     -> cat1 a1 (c, b))
     -> t4 -> (t1 -> cat a t2) -> cat1 a1 c

switchCore sw cur cont = sw cur (arr test) cont' >>> arr fst
  where
    test (_, (_, evt)) = evt
    cont' _ t = cont t >>> arr (\y -> (y, noEvent))

switch :: 
    ArrowApply a => 
    ProcessA a b (c, Event t) -> 
    (t -> ProcessA a b c) ->
    ProcessA a b c

switch = switchCore kSwitch


dSwitch :: 
    ArrowApply a => 
    ProcessA a b (c, Event t) -> 
    (t -> ProcessA a b c) ->
    ProcessA a b c

dSwitch = switchCore dkSwitch


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

kSwitch sf test k = ProcessA {
    feed = proc x ->
      do
        (y, sf') <- feed sf -< x
        (evt, test') <- feed test -< (x, y)
        (case evt
          of
            Event t -> feed (k sf' t)
            _ -> arr $ const (y, kSwitch sf' test' k))
                -<< x,
    sweep = proc x ->
      do
        (my, sf') <- sweep sf -< x
        (mevt, test') <-
            maybe
                (arr $ const (Nothing, test))
                (\y -> arr (,y) >>> sweep test)
                my -<< x
        (case mevt
          of
            Just (Event t) -> sweep (k sf' t)
            _ -> arr $ const (my, kSwitch sf' test' k))
                -<< x,
    suspend = suspend sf
  }



dkSwitch ::
    ArrowApply a => 
    ProcessA a b c ->
    ProcessA a (b, c) (Event t) ->
    (ProcessA a b c -> t -> ProcessA a b c) ->
    ProcessA a b c

dkSwitch sf test k = ProcessA {
    feed = proc x ->
      do
        (y, sf') <- feed sf -< x
        (evt, test') <- feed test -< (x, y)
        (case evt
          of
            Event t -> arr $ const (y, k sf' t)
            _ -> arr $ const (y, dkSwitch sf' test' k))
                -<< x,
    sweep = proc x ->
      do
        (my, sf') <- sweep sf -< x
        (mevt, test') <-
            maybe
                (arr $ const (Nothing, test))
                (\y -> arr (,y) >>> sweep test)
                my -<< x
        (case mevt
          of
            Just (Event t) -> arr $ const (my, k sf' t)
            _ -> arr $ const (my, dkSwitch sf' test' k))
                -<< x,
    suspend = suspend sf
  }
{-


broadcast :: 
    Functor col =>
    b -> col sf -> col (b, sf)

broadcast x sfs = fmap (\sf -> (x, sf)) sfs


par ::
    (ArrowApply a, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessA a ext c) ->
    ProcessA a b (col c)

par r sfs = ProcessA $ parCore r sfs >>> arr cont
  where
    cont (ph, ys, sfs') = (ph, ys, par r sfs')

parB ::
    (ArrowApply a, Tv.Traversable col) =>
    col (ProcessA a b c) ->
    ProcessA a b (col c)

parB = par broadcast

parCore ::
    (ArrowApply a, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessA a ext c) ->
    a (Phase, b) (Phase, col c, col (ProcessA a ext c))

parCore r sfs = proc (ph, x) ->
  do
    let input = r x sfs

    ret <- unwrapArrow (Tv.sequenceA (fmap (WrapArrow . appPh) input)) -<< ph

    let ph' = Fd.foldMap getPh ret
        zs = fmap getZ ret
        sfs' = fmap getSf ret

    returnA -< (ph', zs, sfs')

  where
    appPh (y, sf) = proc ph -> step sf -< (ph, y)

    getPh (ph, _, _) = ph
    getZ (_, z, _) = z
    getSf (_, _, sf) = sf


pSwitch ::
    (ArrowApply a, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessA a ext c) ->
    ProcessA a (b, col c) (Event mng) ->
    (col (ProcessA a ext c) -> mng -> ProcessA a b (col c)) ->
    ProcessA a b (col c)

pSwitch r sfs test k = ProcessA $ proc (ph, x) ->
  do
    (ph', zs, sfs') <- parCore r sfs -<< (ph, x)
    (phT, evt, test') <- step test -< (ph', (x, zs))

    evMaybePh
        (arr $ const (phT, zs, pSwitch r sfs' test' k))
        (step . (k sfs') )
        (phT, evt)
            -<< (ph, x)

pSwitchB ::
    (ArrowApply a, Tv.Traversable col) =>
    col (ProcessA a b c) ->
    ProcessA a (b, col c) (Event mng) ->
    (col (ProcessA a b c) -> mng -> ProcessA a b (col c)) ->
    ProcessA a b (col c)

pSwitchB = pSwitch broadcast


rpSwitch ::
    (ArrowApply a, Tv.Traversable col) =>
    (forall sf. (b -> col sf -> col (ext, sf))) ->
    col (ProcessA a ext c) ->
    ProcessA a (b, Event (col (ProcessA a ext c) -> col (ProcessA a ext c)))
        (col c)

rpSwitch r sfs = ProcessA $ proc (ph, (x, evCont)) ->
  do
    let sfsNew = evMaybePh sfs ($sfs) (ph, evCont)
    (ph', ws, sfs') <- parCore r sfsNew -<< (ph, x)
    returnA -< (ph' `mappend` Suspend, ws, rpSwitch r sfs')


rpSwitchB ::
    (ArrowApply a, Tv.Traversable col) =>
    col (ProcessA a b c) ->
    ProcessA a (b, Event (col (ProcessA a b c) -> col (ProcessA a b c)))
        (col c)

rpSwitchB = rpSwitch broadcast

-- `dpSwitch` and `drpSwitch` are not implemented.

-}
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
        feed = p >>> arr (\y -> (Event y, go)),
        sweep = p >>> arr (\y -> (if nullFd y then Nothing else Just (Event y), go)),
        suspend = const NoEvent
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
        yields :: fa, -- [a] or Maybe a
        hasConsumed :: Bool,
        hasStopped :: Bool
      }
    deriving (Eq, Show)

instance
    Alternative f => Monoid (ExecInfo (f a))
  where
    mempty = ExecInfo empty False False
    ExecInfo y1 c1 s1 `mappend` ExecInfo y2 c2 s2 = 
        ExecInfo (y1 <|> y2) (c1 || c2) (s1 || s2)


-- | Execute until an input consumed and the machine suspended.
stepRun :: 
    ArrowApply a =>
    ProcessA a (Event b) (Event c) ->
    a b (ExecInfo [c], ProcessA a (Event b) (Event c))
stepRun = undefined
{-
stepRun pa0 = unArrowMonad $ \x ->
  do
    (pa, wer)  <- runRM arrowMonad pa0 $ runWriterT $ 
      do
        sweepAll singleton
        _ <- lift $ feed x
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
-}
-- | Execute until an output produced.
stepYield :: 
    ArrowApply a =>
    ProcessA a (Event b) (Event c) ->
    a b (ExecInfo (Maybe c), ProcessA a (Event b) (Event c))
stepYield = undefined
{-
stepYield pa0 = unArrowMonad $ \x -> runRM arrowMonad pa0 $ evalStateT `flip` mempty $
  do
    go x
    r <- get
    pa <- lift freeze
    return (r, pa)

  where 
    go x =
      do
        csmd <- lift $ feed x
        modify $ \ri -> ri { hasConsumed = csmd }
                             
        evo <- lift sweep
        
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


-}
