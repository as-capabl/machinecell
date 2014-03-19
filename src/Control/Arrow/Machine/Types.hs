{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module
    Control.Arrow.Machine.Types
where

import qualified Control.Category as Cat
import Data.Monoid (Monoid, mappend, mempty)
import Data.Profunctor (Profunctor, dimap)
import Control.Monad (liftM)
import Control.Arrow
import Control.Applicative
import Debug.Trace
import Data.Maybe

import Control.Arrow.Machine.Event
import Control.Arrow.Machine.Detail


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

data ProcessA a b c = ProcessA { 
      step :: a (Phase, b) (Phase, c, ProcessA a b c) 
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
    g . f = ProcessA $ concatStep (step f) (step g)
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

arrStep :: ArrowApply a => (b->c) -> a (Phase, b) (Phase, c, ProcessA a b c)
arrStep f = proc (ph, x) ->
    returnA -< (ph `mappend` Suspend, f x, ProcessA $ arrStep f)
{-# INLINE [1] arrStep #-}

concatStep :: ArrowApply a => 
              StepType a b d ->
              StepType a d c ->
              StepType a b c
concatStep f g = proc (ph, x) -> concatStep' ph f g -<< (ph, x)
{-# INLINE [1] concatStep #-}

concatStep' :: ArrowApply a => 
              Phase -> 
              StepType a b d -> StepType a d c -> StepType a b c
             
concatStep' Sweep f g = proc (_, x) ->
  do
    (ph1, r1, _) <- f -< (Suspend, x)
    (ph2, r2, pb') <- g -<< (Sweep, r1)
    cont ph2 x -<< (ph2, r2, ProcessA f >>> pb')
  where
    cont Feed x = returnA
    cont Sweep x = returnA
    cont Suspend x = proc _ ->
      do
        (ph1, r1, pa') <- f -< (Sweep, x)
        (ph2, r2, pb') <- g -< (ph1, r1)
        returnA -< (ph2, r2, pa' >>> pb')

concatStep' ph f g = proc (_, x) ->
  do
    (ph1, r1, pa') <- f -< (ph, x)
    (ph2, r2, pb') <- g -<< (ph1, r1)
    returnA -< (ph2, r2, pa' >>> pb')

-- rules
{-# RULES
"ProcessA: concat/concat" 
    forall f g h. concatStep (concatStep f g) h = concatStep f (concatStep g h)
"ProcessA: arr/arr"
    forall f g. concatStep (arrStep f) (arrStep g) = arrStep (g . f)
"ProcessA: arr/*"
    forall f g. concatStep (arrStep f) g = dimapStep f id g
"ProcessA: */arr"
    forall f g. concatStep f (arrStep g) = dimapStep id g f
"ProcessA: dimap/dimap"
    forall f g h i j. dimapStep f j (dimapStep g i h)  = dimapStep (g . f) (j . i) h
"ProcessA: dimap/arr"
    forall f g h. dimapStep f h (arrStep g) = arrStep (h . g . f)
"ProcessA: par/par"
    forall f1 f2 g1 g2 h. concatStep (parStep f1 f2) (concatStep (parStep g1 g2) h) =
        concatStep (parStep (concatStep f1 g1) (concatStep f2 g2)) h
"ProcessA: par/par-2"
    forall f1 f2 g1 g2. concatStep (parStep f1 f2) (parStep g1 g2) =
        parStep (concatStep f1 g1) (concatStep f2 g2)
  #-}


runProcessA :: ArrowApply a => ProcessA a (Event b) (Event c) -> a [b] [c]
runProcessA pa = proc xs -> 
  do
    ys <- go Sweep pa xs id -<< ()
    returnA -< ys []
  where
    go Sweep pa [] ys = proc _ ->
      do
        (ph', y, pa') <- step pa -< (Sweep, End)
        react y ph' pa' [] ys -<< ()

    go Feed pa [] ys = arr $ const ys

    go ph pa (x:xs) ys = proc _ ->
      do
        let (evx, xs') = if ph == Feed then (Event x, xs) else (NoEvent, x:xs)
        (ph', y, pa') <- step pa -< (ph, evx)
        react y ph' pa' xs' ys -<< ()
    
    react End ph pa xs ys =
      do
        go (adv ph) pa [] ys

    react (Event y) ph pa xs ys =
        go (adv ph) pa xs (\cont -> ys (y:cont))

    react NoEvent ph pa xs ys =
        go (adv ph) pa xs ys

    adv Feed = Sweep
    adv Suspend = Feed


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


stopped :: (ArrowApply a, Occasional c) => ProcessA a b c
stopped = arr (const end)
