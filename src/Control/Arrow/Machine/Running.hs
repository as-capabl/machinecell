{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}

module
    Control.Arrow.Machine.Running
      (
        -- * Run at once.
        run,
        -- * Run step-by-step.
        ExecInfo(..),
        stepRun,
        stepYield
      )
where

import Control.Arrow
import Control.Applicative (Alternative (..))
import Data.Monoid (Monoid (..))

import Control.Arrow.Machine.Types
import Control.Arrow.Machine.Event


adv Feed = Sweep
adv Suspend = Feed


handle f1 f2 f3 = proc (e, (ph, ev)) ->
    handleImpl ph ev -<< e
  where
    handleImpl Feed (Event x) = proc e -> f1 -< (e, x)
    handleImpl Suspend _ = f3
    handleImpl _ End = f3
    handleImpl _ _ = f2


run :: ArrowApply a => ProcessA a (Event b) (Event c) -> a [b] [c]
run pa = proc xs -> 
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

stepRun :: 
    ArrowApply a =>
    ProcessA a (Event b) (Event c) ->
    a b (ExecInfo [c], ProcessA a (Event b) (Event c))



stepRun pa = proc x ->
  do
    (ys1, pa', _) <- go pa id -<< (Sweep, NoEvent)
    (ys2, pa'', hsS) <- go pa' ys1 -<< (Feed, (Event x))
    returnA -< (ExecInfo { yields = ys2 [], hasConsumed = True, hasStopped = hsS } , pa'')

  where
    go pa ys = step pa >>> proc (ph', evy, pa') ->
      do
        (| handle
            (\y -> go pa' (\cont -> ys (y:cont)) -<< (adv ph', NoEvent))
            (go pa' ys -<< (adv ph', NoEvent))
            (returnA -< (ys, pa', case evy of {End->True; _->False}))
         |)
            (ph', evy)

                     
stepYield :: 
    ArrowApply a =>
    ProcessA a (Event b) (Event c) ->
    a b (ExecInfo (Maybe c), ProcessA a (Event b) (Event c))

stepYield pa = proc x ->
  do
    (my, pa', hsS) <- go pa -<< (Sweep, NoEvent)
    (| handle2 
        (returnA -< (ExecInfo { yields = my, hasConsumed = False, hasStopped = hsS}, pa'))
        (do
            (my2, pa'', hsS) <- go pa' -<< (Feed, (Event x))
            returnA -< (ExecInfo { yields = my2, hasConsumed = True, hasStopped = hsS}, pa''))
     |)
        my

  where
    go pa = step pa >>> proc (ph', evy, pa') ->
      do
        (| handle
            (\y -> returnA -<< (Just y, pa', False))
            (go pa' -<< (adv ph', NoEvent))
            (returnA -< (Nothing, pa', case evy of {End->True; _->False}))
         |)
            (ph', evy)


    handle2 f1 f2 = proc (e, mx) ->
        maybe f2 (const f1) mx -<< e

