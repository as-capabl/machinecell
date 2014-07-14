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
import Control.Arrow.Machine.Event.Internal (Event(..))


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
{-
    -- Converted code below with arrowp.
    -- Need refactoring overall this file, rather than rewrite here.

    go pa ys = step pa >>> proc (ph', evy, pa') ->
      do
        (| handle
            (\y -> go pa' (\cont -> ys (y:cont)) -<< (adv ph', NoEvent))
            (go pa' ys -<< (adv ph', NoEvent))
            (returnA -< (ys, pa', case evy of {End->True; _->False}))
         |)
            (ph', evy)
-}
    go pa ys
          = step pa >>>
              (arr (\ (ph', evy, pa') -> ((evy, pa', ph'), (ph', evy))) >>>
                 handle
                   (arr
                      (\ ((evy, pa', ph'), y) ->
                         (go pa' (\ cont -> ys (y : cont)), (adv ph', NoEvent)))
                      >>> app)
                   (arr (\ (evy, pa', ph') -> (go pa' ys, (adv ph', NoEvent))) >>>
                      app)
                   (arr
                      (\ (evy, pa', ph') ->
                         (ys, pa',
                          case evy of
                              End -> True
                              _ -> False))))

                     
stepYield :: 
    ArrowApply a =>
    ProcessA a (Event b) (Event c) ->
    a b (ExecInfo (Maybe c), ProcessA a (Event b) (Event c))

stepYield pa = proc x ->
  do
    (my, pa', hsS) <- go pa -<< (Sweep, NoEvent)
    cont my pa' hsS -<< x

  where
    cont (Just y) pa' hsS = proc _ ->
        returnA -< (ExecInfo { yields = Just y, hasConsumed = False, hasStopped = hsS}, pa')

    cont Nothing pa' hsS = proc x ->
      do
        (my2, pa'', hsS') <- go pa' -<< (Feed, (Event x))
        returnA -< (ExecInfo { yields = my2, hasConsumed = True, hasStopped = hsS}, pa'')
{-
    -- Converted code below with arrowp.
    -- Need refactoring overall this file, rather than rewrite here.

    go pa = step pa >>> proc (ph', evy, pa') ->
      do
        (| handle
            (\y -> returnA -<< (Just y, pa', False))
            (go pa' -<< (adv ph', NoEvent))
            (returnA -< (Nothing, pa', case evy of {End->True; _->False}))
         |)
            (ph', evy)
-}
    go pa = step pa >>>
              (arr (\ (ph', evy, pa') -> ((evy, pa', ph'), (ph', evy))) >>>
                 handle (arr (\ ((evy, pa', ph'), y) -> (Just y, pa', False)))
                   (arr (\ (evy, pa', ph') -> (go pa', (adv ph', NoEvent))) >>> app)
                   (arr
                      (\ (evy, pa', ph') ->
                         (Nothing, pa',
                          case evy of
                              End -> True
                              _ -> False))))



