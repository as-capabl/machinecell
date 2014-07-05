{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module
    Control.Arrow.Machine.Types
    -- This file includes internals. Export definitions is at ../Machine.hs
where

import qualified Control.Category as Cat
import Data.Monoid (Monoid(..))
import Data.Profunctor (Profunctor, dimap)
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

-- | The stream transducer arrow.
--
-- To construct `ProcessA` instances, use `Control.Arrow.Machine.Plan.Plan`,
-- `arr`, functions declared in `Control.Arrow.Machine.Utils`,
-- or arrow combinations of them.
--
-- May use `ArrowChoice` and `ArrowLoop` instance too.
-- but there is a limitation that `loop` cannot propagate `Event`s to upstream.
-- In such case, use `Control.Arrow.Machine.Utils.feedback` instead.
data ProcessA a b c = ProcessA { 
      step :: StepType a b c
    }

fit :: (Arrow a, Arrow a') => 
       (forall p q. a p q -> a' p q) -> 
       ProcessA a b c -> ProcessA a' b c
fit f (ProcessA af) = ProcessA $ f af >>> arr mod
  where
    mod (ph, y, next) = (ph, y, fit f next)


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
{-# INLINE [1] dimapStep #-}


instance
    ArrowApply a => Cat.Category (ProcessA a)
  where
    id = ProcessA (arrStep id)
    {-# INLINE id #-}
    g . f = ProcessA $ compositeStep (step f) (step g)
    {-# INLINE (.) #-}


instance 
    ArrowApply a => Arrow (ProcessA a)
  where
    arr = ProcessA . arrStep
    {-# INLINE arr #-}

    first pa = ProcessA $ proc (ph, (x, d)) ->
      do
        (ph', y, pa') <- step pa -< (ph, x)
        returnA -< (ph' `mappend` Suspend, (y, d), first pa')
    {-# INLINE first #-}

    pa *** pb = ProcessA $ parStep (step pa) (step pb)
    {-# INLINE (***) #-}


parStep f g = proc (ph, (x1, x2)) ->
  do
    (ph1, y1, pa') <- f -< (ph, x1)
    (ph2, y2, pb') <- g -< (ph, x2)
    returnA -< (ph1 `mappend` ph2, (y1, y2), pa' *** pb')
{-# INLINE [1] parStep #-}

arrStep :: ArrowApply a => (b->c) -> StepType a b c
arrStep f = proc (ph, x) ->
    returnA -< (ph `mappend` Suspend, f x, ProcessA $ arrStep f)
{-# INLINE [1] arrStep #-}


-- |Composition is proceeded by the backtracking strategy.
compositeStep :: ArrowApply a => 
              StepType a b d -> StepType a d c -> StepType a b c
compositeStep f g = proc (ph, x) -> compositeStep' ph f g -<< (ph, x)
{-# INLINE [1] compositeStep #-}

compositeStep' :: ArrowApply a => 
              Phase -> 
              StepType a b d -> StepType a d c -> StepType a b c
             
compositeStep' Sweep f g = proc (_, x) ->
  do
    (ph1, r1, pa') <- f -< (Suspend, x)
    (ph2, r2, pb') <- g -<< (Sweep, r1)
    cont ph2 x -<< (r2, pa', pb')
  where
    cont Feed x = arr $ \(r, pa, pb) -> (Feed, r, pa >>> pb)
    cont Sweep x = arr $ \(r, pa, pb) -> (Sweep, r, pa >>> pb)
    cont Suspend x = proc (_, pa, pb) ->
      do
        (ph1, r1, pa') <- step pa -<< (Sweep, x)
        (ph2, r2, pb') <- step pb -<< (ph1, r1)
        returnA -< (ph2, r2, pa' >>> pb')

compositeStep' ph f g = proc (_, x) ->
  do
    (ph1, r1, pa') <- f -< (ph, x)
    (ph2, r2, pb') <- g -<< (ph1, r1)
    returnA -< (ph2, r2, pa' >>> pb')

-- rules
{-# RULES
"ProcessA: concat/concat" 
    forall f g h. compositeStep (compositeStep f g) h = compositeStep f (compositeStep g h)
"ProcessA: arr/arr"
    forall f g. compositeStep (arrStep f) (arrStep g) = arrStep (g . f)
"ProcessA: arr/*"
    forall f g. compositeStep (arrStep f) g = dimapStep f id g
"ProcessA: */arr"
    forall f g. compositeStep f (arrStep g) = dimapStep id g f
"ProcessA: dimap/dimap"
    forall f g h i j. dimapStep f j (dimapStep g i h)  = dimapStep (g . f) (j . i) h
"ProcessA: dimap/arr"
    forall f g h. dimapStep f h (arrStep g) = arrStep (h . g . f)
"ProcessA: par/par"
    forall f1 f2 g1 g2 h. compositeStep (parStep f1 f2) (compositeStep (parStep g1 g2) h) =
        compositeStep (parStep (compositeStep f1 g1) (compositeStep f2 g2)) h
"ProcessA: par/par-2"
    forall f1 f2 g1 g2. compositeStep (parStep f1 f2) (parStep g1 g2) =
        parStep (compositeStep f1 g1) (compositeStep f2 g2)
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
    loop pa = ProcessA $ proc (ph, x) -> loop $ go ph -<< x
      where
        go ph = proc (x, d) ->
          do 
            (ph', (y, d'), pa') <- step pa -< (ph, (x, d))
            returnA -< ((ph', y, loop pa'), d')


