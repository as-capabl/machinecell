{-# LANGUAGE Safe #-}
{-# LANGUAGE Arrows #-}

module
    Control.Arrow.Machine.Evolution
      (
        switchAfter,
        dSwitchAfter,
        kSwitchAfter,
        dkSwitchAfter,
        finishWith,
        evolve
      )
where

import Prelude hiding (id, (.))
import Data.Void
import Control.Category
import Control.Arrow.Machine.Types
import Control.Monad.Cont (cont, runCont)

switchAfter ::
    Monad m =>
    ProcessT m i (o, Event r) ->
    Evolution i o m r
switchAfter pf = Evolution $ cont $ switch pf

dSwitchAfter ::
    Monad m =>
    ProcessT m i (o, Event r) ->
    Evolution i o m r
dSwitchAfter pf = Evolution $ cont $ dSwitch pf

kSwitchAfter ::
    Monad m =>
    ProcessT m i o ->
    ProcessT m (i, o) (Event r) ->
    Evolution i o m (ProcessT m i o, r)
kSwitchAfter pf test = Evolution $ cont $ kSwitch pf test . curry

dkSwitchAfter ::
    Monad m =>
    ProcessT m i o ->
    ProcessT m (i, o) (Event r) ->
    Evolution i o m (ProcessT m i o, r)
dkSwitchAfter pf test = Evolution $ cont $ dkSwitch pf test . curry

finishWith ::
    Monad m =>
    ProcessT m i o ->
    Evolution i o m r
finishWith pf = Evolution $ cont $ const pf

evolve ::
    Evolution i o m Void ->
    ProcessT m i o
evolve ev = runCont (runEvolution ev) absurd
