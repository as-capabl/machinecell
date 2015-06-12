{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module
    Control.Arrow.Machine.Types
      (
        -- * Basic types
        ProcessA(),

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
        pSwitch,
        pSwitchB,
        rpSwitch,
        rpSwitchB,
        par,
        parB,

        
        -- * Primitive machines - other safe primitives
        fit,
        fitEx,
        loop',
        
        -- * Primitive machines - unsafe
        unsafeSteady,
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
import qualified Control.Monad.Trans.Free as F
import qualified Control.Monad.Trans.Free.Church as F
import Control.Arrow.Machine.ArrowUtil
import Control.Arrow.Machine.Plan.Internal


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


type StepType a b c = a (Phase, b) (Phase, c, ProcessA a b c) 

-- | The stream transducer arrow.
--
-- To construct `ProcessA` instances, use `Control.Arrow.Machine.Plan.Plan`,
-- `arr`, functions declared in `Control.Arrow.Machine.Utils`,
-- or arrow combinations of them.
--
-- May use `ArrowChoice` and `ArrowLoop` instance too.
-- but there is a limitation that `loop` cannot propagate `Event`s to upstream.
-- In such case, use `Control.Arrow.Machine.Utils.feedback` instead.
newtype ProcessA a b c = ProcessA { 
      step :: StepType a b c
    }


fitEx :: (Arrow a, Arrow a') =>
    (forall p q. a (p, b) (q, c) -> a' (p, b') (q, c')) ->
    ProcessA a b c ->
    ProcessA a' b' c'
fitEx f k = ProcessA $ proc (ph, x) ->
  do
    ((ph', k'), y) <- f (step k >>> arr (\(ph', y, k') -> ((ph', k'), y))) -< (ph, x)
    returnA -< (ph', y, fitEx f k')


fit :: (Arrow a, Arrow a') => 
       (forall p q. a p q -> a' p q) -> 
       ProcessA a b c -> ProcessA a' b c
fit f = fitEx f


loop' :: ArrowApply a =>
    d ->
    ProcessA a (b, d) (c, d) ->
    ProcessA a b c
loop' i pa = ProcessA $ proc (ph, x) ->
  do
    (ph', (y, n), pa') <- step pa -< (ph, (x, i))
    returnA -< (ph', y, loop' n pa')

instance
    Arrow a => Profunctor (ProcessA a)
  where
    dimap f g pa = ProcessA $ dimapStep f g (step pa)
    {-# INLINE dimap #-}

dimapStep :: Arrow a => 
             (b->c)->(d->e)->
             StepType a c d -> StepType a b e
dimapStep f g stp = proc (ph, x) ->
  do
    (ph', y, pa') <- stp -< (ph, f x)
    returnA -< (ph', g y, dimap f g pa')
{-# NOINLINE dimapStep #-}

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
    id = ProcessA idStep
    {-# INLINE id #-}
    g . f = ProcessA $ compositeStep (step f) (step g)
    {-# INLINE (.) #-}


instance 
    ArrowApply a => Arrow (ProcessA a)
  where
    arr = ProcessA . arrStep
    {-# INLINE arr #-}

    first pa = ProcessA $ parStep (step pa) idStep
    {-# INLINE first #-}

    second pa = ProcessA $ parStep idStep (step pa)
    {-# INLINE second #-}

    pa *** pb = ProcessA $ parStep (step pa) (step pb)
    {-# INLINE (***) #-}


parStep :: ArrowApply a =>
    StepType a b c ->
    StepType a d e ->
    StepType a (b, d) (c, e)
parStep f g = proc (ph, (x1, x2)) ->
  do
    (ph1, y1, pa') <- f -< (ph, x1)
    (ph2, y2, pb') <- g -< (ph, x2)
    returnA -< (ph1 `mappend` ph2, (y1, y2), pa' *** pb')
{-# NOINLINE parStep #-}

idStep :: ArrowApply a => StepType a b b
idStep = proc (ph, x) ->
    returnA -< (ph `mappend` Suspend, x, ProcessA $ idStep)
{-# NOINLINE idStep #-}

arrStep :: ArrowApply a => (b->c) -> StepType a b c
arrStep f = proc (ph, x) ->
    returnA -< (ph `mappend` Suspend, f x, ProcessA $ arrStep f)
{-# NOINLINE arrStep #-}


-- |Composition is proceeded by the backtracking strategy.
compositeStep :: ArrowApply a => 
              StepType a b d -> StepType a d c -> StepType a b c
compositeStep f g = proc (ph, x) -> compositeStep' ph f g -<< (ph, x)
{-# NOINLINE compositeStep #-}

compositeStep' :: ArrowApply a => 
              Phase -> 
              StepType a b d -> StepType a d c -> StepType a b c
             
compositeStep' Sweep f g = proc (_, x) ->
  do
    (_, r1, pa') <- f -< (Suspend, x)
    (ph2, r2, pb') <- g -<< (Sweep, r1)
    cont ph2 -<< (r2, pa', pb', x)
  where
    cont Feed = arr $ \(r, pa, pb, _) -> (Feed, r, pa >>> pb)
    cont Sweep = arr $ \(r, pa, pb, _) -> (Sweep, r, pa >>> pb)
    cont Suspend = proc (r, pa, pb, x) ->
      do
        (ph1, r1, pa') <- step pa -<< (Sweep, x)
        (ph2, r2, pb') <-
            (if ph1 == Feed
                then
                  step pb
                else
                  arr $ const (Suspend, r, pb))
                      -<< (ph1, r1)
        returnA -< (ph2, r2, pa' >>> pb')

compositeStep' ph f g = proc (_, x) ->
  do
    (ph1, r1, pa') <- f -< (ph, x)
    (ph2, r2, pb') <- g -<< (ph1, r1)
    returnA -< (ph2, r2, pa' >>> pb')

-- rules
{-# RULES
"ProcessA: id/*"
    forall g. compositeStep idStep g = g
"ProcessA: */id"
    forall f. compositeStep f idStep = f

"ProcessA: concat/concat" 
    forall f g h. compositeStep (compositeStep f g) h = compositeStep f (compositeStep g h)

"ProcessA: dimap/dimap"
    forall f g h i j. dimapStep f j (dimapStep g i h)  = dimapStep (g . f) (j . i) h
"ProcessA: dimap/arr"
    forall f g h. dimapStep f h (arrStep g) = arrStep (h . g . f)

"ProcessA: arr***/par"
    forall f1 f2 g1 g2 h. compositeStep (parStep f1 (arrStep f2)) (compositeStep (parStep g1 g2) h) =
        compositeStep (parStep (compositeStep f1 g1) (dimapStep f2 id g2)) h
"ProcessA: arr***/par-2"
    forall f1 f2 g1 g2. compositeStep (parStep f1 (arrStep f2)) (parStep g1 g2) =
        parStep (compositeStep f1 g1) (dimapStep f2 id g2)
"ProcessA: par/***arr"
    forall f1 f2 g1 g2 h. compositeStep (parStep f1 f2) (compositeStep (parStep (arrStep g1) g2) h) =
        compositeStep (parStep (dimapStep id g1 f1) (compositeStep f2 g2)) h
"ProcessA: par/***arr-2"
    forall f1 f2 g1 g2. compositeStep (parStep f1 f2) (parStep (arrStep g1) g2) =
        parStep (dimapStep id g1 f1) (compositeStep f2 g2)

"ProcessA: first/par"
    forall f1 g1 g2 h. compositeStep (parStep f1 idStep) (compositeStep (parStep g1 g2) h) =
        compositeStep (parStep (compositeStep f1 g1) g2) h
"ProcessA: first/par-2"
    forall f1 g1 g2. compositeStep (parStep f1 idStep) (parStep g1 g2) =
        parStep (compositeStep f1 g1) g2
"ProcessA: par/second"
    forall f1 f2 g2 h. compositeStep (parStep f1 f2) (compositeStep (parStep idStep g2) h) =
        compositeStep (parStep f1 (compositeStep f2 g2)) h
"ProcessA: par/second-2"
    forall f1 f2 g2. compositeStep (parStep f1 f2) (parStep idStep g2) =
        parStep f1 (compositeStep f2 g2)

"ProcessA: arr/arr"
    forall f g h. compositeStep (arrStep f) (compositeStep (arrStep g) h) =
        compositeStep (arrStep (g . f)) h
"ProcessA: arr/arr-2"
    forall f g. compositeStep (arrStep f) (arrStep g) = arrStep (g . f)
"ProcessA: arr/*" [1]
    forall f g. compositeStep (arrStep f) g = dimapStep f id g
"ProcessA: */arr" [1]
    forall f g. compositeStep f (arrStep g) = dimapStep id g f
"ProcessA: arr***arr" [0]
    forall f g. parStep (arrStep f) (arrStep g) = arrStep (f *** g)
  #-}

instance
    ArrowApply a => ArrowChoice (ProcessA a)
  where
    left pa@(ProcessA a) = ProcessA $ proc (ph, eth) -> go ph eth -<< ()
      where
        go ph (Left x) = proc _ -> 
          do
            (ph', y, pa') <- a -< (ph, x)
            returnA -< (ph', Left y, left pa')
        go ph (Right d) = proc _ -> 
            returnA -< (ph `mappend` Suspend, Right d, left pa)

instance
    (ArrowApply a, ArrowLoop a) => ArrowLoop (ProcessA a)
  where
    loop = fitEx (\f -> loop (lp f))
      where
        lp f = proc ((p, x), d) ->
          do
            (q, (y, d')) <- f -< (p, (x, d))
            returnA -< ((q, y), d')


instance
    (ArrowApply a, ArrowReader r a) => 
    ArrowReader r (ProcessA a)
  where
    readState = ProcessA $ proc (ph, dm) ->
      do
        r <- readState -< dm
        returnA -< (ph `mappend` Suspend, r, readState)

    newReader = fitEx nr
      where
        nr f = proc (p, (x, r)) -> newReader f -< ((p, x), r)

instance
    (ArrowApply a, ArrowApply a', ArrowAddReader r a a') =>
    ArrowAddReader r (ProcessA a) (ProcessA a')
  where
    liftReader pa = ProcessA $ proc (ph, x) ->
      do
        (ph', y, pa') <- (| liftReader (step pa -< (ph, x)) |)
        returnA -< (ph', y, liftReader pa')

    elimReader pra = 
        ProcessA $ arr pre >>> elimReader (step pra) >>> arr post
      where
        pre (ph, (x, r)) = ((ph, x), r)
        post (ph, x, pra') = (ph, x, elimReader pra')


    
data Event a = Event a | NoEvent | End deriving (Eq, Show)


instance 
    Functor Event 
  where
    fmap _ NoEvent = NoEvent
    fmap _ End = End
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
    traverse _ NoEvent = Ap.pure NoEvent
    traverse _ End = Ap.pure End

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

{-
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
-}



evMaybe :: Arrow a => c -> (b->c) -> a (Event b) c
evMaybe r f = arr go
  where
    go (Event x) = f x
    go NoEvent = r
    go End = r


{-
fromEvent :: Arrow a => b -> a (Event b) b
fromEvent x = evMaybe x id
-}

-- TODO: テスト
condEvent :: Bool -> Event a -> Event a
condEvent _ End = End
condEvent True ev = ev
condEvent False _ = NoEvent

-- TODO: テスト
filterEvent :: (a -> Bool) -> Event a -> Event a
filterEvent cond ev@(Event x) = condEvent (cond x) ev
filterEvent _ ev = ev

evMap ::  Arrow a => (b->c) -> a (Event b) (Event c)
evMap = arr . fmap

{-
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
-}



stopped :: 
    (ArrowApply a, Occasional c) => ProcessA a b c
stopped = arr (const end)


muted ::
    (ArrowApply a, Occasional b) => ProcessA a b (Event c)
muted = arr collapse >>> repeatedly await


yield :: o -> Plan i o ()
yield x = F.liftF $ YieldPF x ()

await :: Plan i o i
await = F.FT $ \pure free -> free (AwaitPF pure (free StopPF))

stop :: Plan i o a
stop = F.liftF $ StopPF






constructT :: (Monad m, ArrowApply a) => 
              (forall b. m b -> a () b) ->
              PlanT i o m r -> 
              ProcessA a (Event i) (Event o)

constructT fit0 pl = ProcessA $ fit' $ F.runFT pl pure free
  where
    fit' ma = proc arg -> do { (evx, pa) <- fit0 ma -< (); modFit evx pa -<< arg }
    
    modFit :: ArrowApply a => Event c -> StepType a b (Event c) -> StepType a b (Event c)
    modFit (Event x) stp = retArrow Feed (Event x) (ProcessA stp)
    modFit End stp = retArrow Feed End (ProcessA stp)
    modFit _ stp = stp

    retArrow ph' evx cont = arr $ \(ph, _) -> 
        case ph of
          Suspend -> 
              (ph `mappend` Suspend,
               if isEnd evx then End else NoEvent,
               ProcessA $ retArrow ph' evx cont)
          _ -> 
              (ph `mappend` ph', evx, cont)

    pure _ = return $ (End, retArrow Suspend End stopped)

    free (AwaitPF f ff) =
      do
        return $ (NoEvent, arr (uncurry (awaitIt f ff)) >>> proc pc -> pc -<< ())

    free (YieldPF y fc) = return $ (Event y, fit' fc)

    free StopPF = return $ (End, retArrow Suspend End stopped)


    awaitIt f _ Feed (Event x) = proc _ ->
      do
        (evy, stp) <- fit0 (f x) -< ()
        returnA -< (Feed, evy, ProcessA stp)

    awaitIt _ ff Feed End = proc _ ->
      do
        (evy, stp) <- fit0 ff -< ()
        returnA -< (Feed, evy, ProcessA stp)

    awaitIt _ ff Sweep End = proc _ ->
      do
        (evy, stp) <- fit0 ff -< ()
        returnA -< (if not $ isNoEvent evy then Feed else Suspend, evy, ProcessA stp)

    awaitIt f ff ph _ = proc _ ->
        returnA -< (ph `mappend` Suspend, NoEvent, 
                    ProcessA $ arr (uncurry (awaitIt f ff)) >>> proc pc -> pc -<< ())


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
evMaybePh :: b -> (a->b) -> (Phase, Event a) -> b
evMaybePh _ f (Feed, Event x) = f x
evMaybePh _ f (Sweep, Event x) = f x
evMaybePh d _ _ = d


{-
type KSwitchLike a b c t =
    ProcessA a b c ->
    ProcessA a (b, ) (Event t) ->
    (ProcessA a b c -> t -> ProcessA a b c) ->
    ProcessA a b c

switchCore ::
    ArrowApply a =>
    KSwitchLike a b c t ->
    ProcessA a b (c, Event t) -> 
    (t -> ProcessA a b c) ->
    ProcessA a b c
-}
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

rSwitch cur = ProcessA $ proc (ph, (x, eva)) -> 
  do
    let now = evMaybePh cur id (ph, eva)
    (ph', y, new) <-  step now -<< (ph, x)
    returnA -< (ph', y, rSwitch new)


drSwitch :: 
    ArrowApply a => ProcessA a b c -> 
    ProcessA a (b, Event (ProcessA a b c)) c

drSwitch cur = ProcessA $ proc (ph, (x, eva)) -> 
  do
    (ph', y, new) <- step cur -< (ph, x)
    
    returnA -< (ph', y, next new eva)

  where
    next _ (Event af) = drSwitch af
    next af _ = drSwitch af


kSwitch ::
    ArrowApply a => 
    ProcessA a b c ->
    ProcessA a (b, c) (Event t) ->
    (ProcessA a b c -> t -> ProcessA a b c) ->
    ProcessA a b c

kSwitch sf test k = ProcessA $ proc (ph, x) ->
  do
    (ph', y, sf') <- step sf -< (ph, x)
    (phT, evt, test') <- step test -< (ph', (x, y))

    evMaybePh 
        (arr $ const (phT, y, kSwitch sf' test' k)) 
        (step . (k sf'))
        (phT, evt)
            -<< (phT, x)


dkSwitch ::
    ArrowApply a => 
    ProcessA a b c ->
    ProcessA a (b, c) (Event t) ->
    (ProcessA a b c -> t -> ProcessA a b c) ->
    ProcessA a b c

dkSwitch sf test k = ProcessA $ proc (ph, x) ->
  do
    (ph', y, sf') <- step sf -< (ph, x)
    (phT, evt, test') <- step test -< (ph', (x, y))
    
    let
        nextA t = k sf' t
        nextB = dkSwitch sf' test' k

    returnA -< (phT, y, evMaybe nextB nextA evt)


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
        (arr $ const (ph' `mappend` phT, zs, pSwitch r sfs' test' k))
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


--
-- Unsafe primitives
--

-- | Repeatedly call `p`.
--
-- How many times `p` is called is indefinite.
-- So `p` must satisfy the equation below;
--
-- @p === p &&& p >>> arr fst === p &&& p >>> arr snd@
unsafeSteady ::
    ArrowApply a =>
    a b c ->
    ProcessA a b c
unsafeSteady p = ProcessA $ proc (ph, x) ->
  do
    y <- p -< x
    returnA -< (ph `mappend` Suspend, y, unsafeSteady p)
  
    
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
    go = ProcessA $ proc (ph, x) -> handle ph -<< x
    
    handle Suspend =
        arr $ const (Suspend, NoEvent, go)

    handle ph = proc x ->
      do
        ys <- p -< x
        let ph' = if nullFd ys then Suspend else Feed
        returnA -< (ph `mappend` ph', Event ys, go)

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
    getContWE :: Bool
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

feed :: 
    Monad m => 
    i -> RM a (Event i) o m Bool
feed x = feed_ (Event x) NoEvent


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
    

sweep :: 
    Monad m =>
    RM a i o m o
sweep =
  do
    pa <- freeze
    fit0 <- gets getFitRI
    ph <- gets getPhaseRI
    x <- if ph == Feed
        then gets getInputRI
        else gets getPaddingRI
    
    (ph', y, pa') <- lift $ fit0 (step pa) (ph, x)
    
    ri <- get
    put $ ri {
        freezeRI = 
            pa',
        getPhaseRI = 
            if ph' == Feed then Sweep else ph'
      }

    return y


sweepAll :: 
    (ArrowApply a, Monoid r, Monad m) =>
    (o->r) ->
    WriterT (WithEnd r) (RM a i (Event o) m) ()
sweepAll outpre = 
        while_ 
            ((not . (== Suspend)) `liftM` lift (gets getPhaseRI)) $
          do
            evx <- lift sweep
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
    (ArrowApply a, Monoid r) =>
    (c -> r) ->
    ProcessA a (Event b) (Event c) ->
    a [b] r
runOn outpre pa0 = unArrowMonad $ \xs ->
  do
    wer <- runRM arrowMonad pa0 $ execWriterT $ 
      do
        go xs
        _ <- lift (feed_ End End)
        sweepAll outpre
    return $ getRWE wer

  where
    go xs =
      do
        (_, wer) <- listen $ sweepAll outpre
        if getContWE wer then cont xs else return ()

    cont [] = return ()

    cont (x:xs) =
      do
        _ <- lift $ feed x
        go xs


-- | Run a machine.
run :: 
    ArrowApply a => 
    ProcessA a (Event b) (Event c) -> 
    a [b] [c]
run pa = 
    runOn (\x -> Endo (x:)) pa >>>
    arr (appEndo `flip` [])

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
