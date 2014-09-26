{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

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
import Control.Monad.State
import Control.Monad.Writer
import Data.Monoid (Monoid (..), Endo(..), appEndo)
import Data.Maybe (fromMaybe)

import Control.Arrow.Machine.Types
import Control.Arrow.Machine.Event
import Control.Arrow.Machine.Event.Internal (Event(..))


--
-- Utilities
--
aToM a i = 
    ArrowMonad $ arr (const i) >>> a
mToA fma = 
    proc x -> case fma x of {ArrowMonad a -> a} -<< ()



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
    mx@(WithEnd x False) `mappend` _ = mx


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


finalizeE :: 
    Monad m => 
    RM a (Event i) o m Bool
finalizeE = feed_ End End


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
    fit <- gets getFitRI
    ph <- gets getPhaseRI
    x <- if ph == Feed
        then gets getInputRI
        else gets getPaddingRI
    
    (ph', y, pa') <- lift $ fit (step pa) (ph, x)
    
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
runOn outpre pa0 = mToA $ \xs ->
  do
    wer <- runRM aToM pa0 $ execWriterT $ 
      do
        go xs
        lift (feed_ End End)
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
        lift $ feed x
        go xs


-- | Run a machine.
run :: 
    ArrowApply a => 
    ProcessA a (Event b) (Event c) -> 
    a [b] [c]
run pa = 
    runOn (\x -> Endo (x:)) pa >>>
    arr (appEndo `flip` [])


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

stepRun pa0 = mToA $ \x ->
  do
    (pa, wer)  <- runRM aToM pa0 $ runWriterT $ 
      do
        sweepAll singleton
        lift $ feed x
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

stepYield pa0 = mToA $ \x -> runRM aToM pa0 $ evalStateT `flip` mempty $
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
                csmd <- gets hasConsumed
                if csmd then return () else go x

            End ->
                modify $ \ri -> ri { hasStopped = True }
