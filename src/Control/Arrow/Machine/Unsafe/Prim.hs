{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module
    Control.Arrow.Machine.Unsafe.Prim
      (
        -- * Unsafe primitives.
        unsafeSteady,
        unsafeExhaust
      )
where

import Data.Monoid (mappend, All(..), getAll)
import Data.Functor ((<$>))
import Data.Maybe (isNothing)
import Control.Monad
import Control.Arrow
import qualified Data.Foldable as Fd

import Control.Arrow.Machine.ArrowUtil
import Control.Arrow.Machine.Types
import Control.Arrow.Machine.Event
import Control.Arrow.Machine.Event.Internal (Event(..))
import Control.Arrow.Machine.Plan


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
        let ph' = if null ys then Suspend else Feed
        returnA -< (ph `mappend` ph', Event ys, go)

    fork = repeatedly $ await >>= Fd.mapM_ yield

    null = getAll . Fd.foldMap (\_ -> All False)

