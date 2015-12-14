{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module
    ConduitAdaptor (
        module Automaton
      )
where

import qualified Control.Arrow.Machine as Machinecell
import qualified Data.Conduit as Conduit
import Data.Conduit ((=$=), ($$+-))
import Control.Monad.Morph (hoist)
import Control.Monad.Trans (lift)
import Control.Monad (forever)

import Automaton

instance
    Monad m =>
    Automaton (Conduit.ConduitM i o m r) m i o
  where
    auto cd =
        let
            cd' = hoist lift cd >> return ()
            cd'' = sourcePlan =$= cd' =$= sinkPlan
          in
            Conduit.runConduit cd''

instance
    Monad m =>
    Automaton (Conduit.ResumableSource m o) m () o
  where
    auto rs =
        hoist lift rs $$+- sinkPlan 

--
-- private
--
sourcePlan ::
    Monad m => Conduit.Source (Machinecell.PlanT a s m) a
sourcePlan = forever $
  do
    x <- lift Machinecell.await
    Conduit.yieldOr x Machinecell.stop

sinkPlan ::
    Monad m => Conduit.Sink a (Machinecell.PlanT s a m) r
sinkPlan = forever $
  do
    mx <- Conduit.await
    maybe (lift Machinecell.stop) (lift . Machinecell.yield) mx

