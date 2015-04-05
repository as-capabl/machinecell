{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module
    Control.Arrow.Machine.Types
    -- This file includes internals. Export definitions is at ../Machine.hs
where

import qualified Control.Category as Cat
import Data.Monoid (Monoid(..))
import qualified Control.Category.Free.Cat as TAS
import Control.Category.Free.View ((<|), (|>))
import Control.Category.Free.Catenated (mapCat)
import qualified Control.Category.Free.View as Vw
import Control.Category.Free.Catenated (foldCat)
import Data.Profunctor (Profunctor, dimap)
import Control.Arrow.Operations (ArrowReader(..))
import Control.Arrow.Transformer.Reader (runReader, ArrowAddReader(..))
import Control.Arrow


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

newtype StepView a b c = StepView {
    viewStep :: StepType a b c
}

data CachedSV a b c = CachedSV {
    viewStepC :: StepType a b c,
    cached :: b
}


-- | The stream transducer arrow.
--
-- To construct `ProcessA` instances, use `Control.Arrow.Machine.Plan.Plan`,
-- `arr`, functions declared in `Control.Arrow.Machine.Utils`,
-- or arrow combinations of them.
--
-- May use `ArrowChoice` and `ArrowLoop` instance too.
-- but there is a limitation that `loop` cannot propagate `Event`s to upstream.
-- In such case, use `Control.Arrow.Machine.Utils.feedback` instead.
data ProcessA a b c = 
    ProcessA (StepType a b c) |
    ProcessSeq (TAS.Cat (StepView a) b c)


-- |Stepping `ProcessA` (internal)
step :: (ArrowApply a) => ProcessA a b c -> StepType a b c

step (ProcessA stp) = stp


step (ProcessSeq tas) = proc (ph, x) ->
  do
    seqStep ph tas -<< x

seqStep :: 
    ArrowApply a => 
    Phase -> 
    (TAS.Cat (StepView a) b c) -> 
    a b (Phase, c, ProcessA a b c)
seqStep Feed tas = proc x ->
  do
    (y, tas') <- traverseStep Feed tas -< x
    returnA -< (Feed, y, ProcessSeq tas')

seqStep Suspend tas = proc x ->
  do
    (y, tas') <- traverseStep Suspend tas -< x
    returnA -< (Suspend, y, ProcessSeq tas')

seqStep Sweep tas = proc x ->
  do
    (yC, tasC) <- makeCache Suspend tas -< x
    (my, tas') <- seqSweepStep Cat.id tasC -<< x
    returnA -< case my
      of
      Just y ->
        (Feed, y, ProcessSeq tas')
      Nothing ->
        (Suspend, yC, ProcessSeq tas')




traverseStep ::
    ArrowApply a =>
    Phase ->
    (TAS.Cat (StepView a) b c) -> 
    a b (c, TAS.Cat (StepView a) b c)
traverseStep ph tas = proc x ->
    traverseArr (traverseOp ph) tas -< (x, Cat.id)


traverseOp :: 
    ArrowApply a =>
    Phase ->
    StepView a p q -> a p (q, StepView a p q)
traverseOp ph sv = proc x ->
  do
    (_, y, pa) <- viewStep sv -< (ph, x)
    returnA -< (y, StepView $ step pa)


traverseArr ::
    Arrow a'' =>
    (forall p q. a p q -> a'' p (q, a' p q)) ->
    (TAS.Cat a d c) -> 
    a'' (d, TAS.Cat a' b d) (c, TAS.Cat a' b c)
traverseArr f tas = 
    case
        Vw.unsnoc tas
      of
      Vw.Empty ->
        Cat.id
      tas' Vw.:| stp ->
        proc (x, rets) ->
          do
            (y, stp') <- f stp -< x
            traverseArr f tas' -< (y, stp' <| rets)

seqSweepStep ::
    ArrowApply a =>
    (TAS.Cat (StepView a) d c) -> 
    (TAS.Cat (CachedSV a) b d) ->
    a b (Maybe c, TAS.Cat (StepView a) b c)
seqSweepStep done cached =
    case
        Vw.uncons cached
      of
      Vw.Empty ->
        proc _ -> returnA -< (Nothing, done)
      CachedSV stp x Vw.:| cached' ->
        proc dm ->
          do
            (ph', y, pa) <- stp -< (Sweep, x)
            let stp' = step pa
            (if ph' == Feed 
              then 
                proc _ ->
                  do
                    let cachedR = StepView stp' Vw.<| mapCat (StepView . viewStepC) cached'
                    (z, doneR) <- traverseArr (traverseOp Feed) done -< (y, cachedR)
                    returnA -< (Just z, doneR)
              else
                proc _ ->
                  do
                    seqSweepStep (done Vw.|> (StepView stp')) cached' -< dm
                ) -<< ()
                      


makeCache ::
    ArrowApply a =>
    Phase ->
    (TAS.Cat (StepView a) b c) -> 
    a b (c, TAS.Cat (CachedSV a) b c)
makeCache ph tas = proc x ->
    traverseArr trans tas -< (x, Cat.id)
  where
    trans :: 
        ArrowApply a =>
        StepView a p q -> a p (q, CachedSV a p q)
    trans sv = proc x ->
      do
        (_, y, pa) <- viewStep sv -< (ph, x)
        returnA -< (y, CachedSV (step pa) x)


toTas :: (ArrowApply a) => ProcessA a b c -> TAS.Cat (StepView a) b c

toTas (ProcessA stp) = TAS.singleton (StepView stp)

toTas (ProcessSeq tas) = tas


-- |Fit(hoist) base arrow to another.
fit :: 
    (Arrow a, Arrow a') => 
    (forall p q. a p q -> a' p q) -> 
    ProcessA a b c -> ProcessA a' b c
fit f (ProcessA af) = 
    ProcessA $ fitStep f af
fit f (ProcessSeq tas) = 
    ProcessSeq $ mapCat (StepView . fitStep f . viewStep) tas

fitStep ::
    (Arrow a, Arrow a') => 
    (forall p q. a p q -> a' p q) -> 
    StepType a b c -> StepType a' b c
fitStep f af = f af >>> arr mod
  where
    mod (ph, y, next) = (ph, y, fit f next)


instance
    ArrowApply a => Profunctor (ProcessA a)
  where
    dimap f g pa = ProcessA $ dimapStep f g (step pa)

dimapStep :: ArrowApply a => 
             (b->c)->(d->e)->
             StepType a c d -> StepType a b e
dimapStep f g stp = proc (ph, x) ->
  do
    (ph', y, pa') <- stp -< (ph, f x)
    returnA -< (ph', g y, dimap f g pa')


instance
    ArrowApply a => Cat.Category (ProcessA a)
  where
    id = ProcessA (arrStep id)
    g . f = ProcessSeq $ toTas g <<< toTas f

instance 
    ArrowApply a => Arrow (ProcessA a)
  where
    arr = ProcessA . arrStep

    first pa = ProcessA $ proc (ph, (x, d)) ->
      do
        (ph', y, pa') <- step pa -< (ph, x)
        returnA -< (ph' `mappend` Suspend, (y, d), first pa')

    pa *** pb = ProcessA $ parStep (step pa) (step pb)


parStep f g = proc (ph, (x1, x2)) ->
  do
    (ph1, y1, pa') <- f -< (ph, x1)
    (ph2, y2, pb') <- g -< (ph, x2)
    returnA -< (ph1 `mappend` ph2, (y1, y2), pa' *** pb')


arrStep :: ArrowApply a => (b->c) -> StepType a b c
arrStep f = proc (ph, x) ->
    returnA -< (ph `mappend` Suspend, f x, ProcessA $ arrStep f)


instance
    ArrowApply a => ArrowChoice (ProcessA a)
  where
    left pa = ProcessA $ proc (ph, eth) -> go ph eth -<< ()
      where
        go ph (Left x) = proc _ -> 
          do
            (ph', y, pa') <- step pa -< (ph, x)
            returnA -< (ph', Left y, left pa')
        go ph (Right d) = proc _ -> 
            returnA -< (ph `mappend` Suspend, Right d, left pa)

instance
    (ArrowApply a, ArrowLoop a) => ArrowLoop (ProcessA a)
  where
    loop pa = ProcessA $ proc (ph, x) -> loop $ go ph -<< x
      where
        go ph = proc (x, d) ->
          do 
            (ph', (y, d'), pa') <- step pa -< (ph, (x, d))
            returnA -< ((ph', y, loop pa'), d')

instance
    (ArrowApply a, ArrowReader r a) => 
    ArrowReader r (ProcessA a)
  where
    readState = ProcessA $ proc (ph, dm) ->
      do
        r <- readState -< dm
        returnA -< (ph `mappend` Suspend, r, readState)

    newReader pa = ProcessA $ proc (ph, (e, r)) ->
      do
        (ph', y, pa') <- newReader (step pa) -< ((ph, e), r)
        returnA -< (ph', y, newReader pa')


instance
    (ArrowApply a, ArrowApply a', ArrowAddReader r a a') =>
    ArrowAddReader r (ProcessA a) (ProcessA a')
  where
    liftReader pa = ProcessA $ proc (ph, x) ->
      do
        (ph', y, pa') <- (| liftReader (step pa -< (ph, x)) |)
        returnA -< (ph', y, liftReader pa)

    elimReader pra = 
        ProcessA $ arr pre >>> elimReader (step pra) >>> arr post
      where
        pre (ph, (x, r)) = ((ph, x), r)
        post (ph, x, pra') = (ph, x, elimReader pra')
