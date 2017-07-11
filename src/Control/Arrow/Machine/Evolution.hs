{-# LANGUAGE Safe #-}
{-# LANGUAGE Arrows #-}

module
    Control.Arrow.Machine.Evolution
      (
        switchAfter,
        dSwitchAfter,
        kSwitchAfter,
        dkSwitchAfter,
        gSwitchAfter,
        dgSwitchAfter,
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
    ProcessT m (i, o) (Event r) ->
    ProcessT m i o ->
    Evolution i o m (ProcessT m i o, r)
kSwitchAfter test pf = Evolution $ cont $ kSwitch pf test . curry

dkSwitchAfter ::
    Monad m =>
    ProcessT m (i, o) (Event r) ->
    ProcessT m i o ->
    Evolution i o m (ProcessT m i o, r)
dkSwitchAfter test pf = Evolution $ cont $ dkSwitch pf test . curry

gSwitchAfter ::
    Monad m =>
    ProcessT m i (p, r) ->
    ProcessT m (q, r) (o, Event t) ->
    ProcessT m p q ->
    Evolution i o m (ProcessT m p q, t)
gSwitchAfter pre post pf = Evolution $ cont $ gSwitch pre pf post . curry

dgSwitchAfter ::
    Monad m =>
    ProcessT m i (p, r) ->
    ProcessT m (q, r) (o, Event t) ->
    ProcessT m p q ->
    Evolution i o m (ProcessT m p q, t)
dgSwitchAfter pre post pf = Evolution $ cont $ dgSwitch pre pf post . curry

finishWith ::
    Monad m =>
    ProcessT m i o ->
    Evolution i o m r
finishWith pf = Evolution $ cont $ const pf

evolve ::
    Evolution i o m Void ->
    ProcessT m i o
evolve ev = runCont (runEvolution ev) absurd
