{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module
    Control.Arrow.Machine.Utils
where

import Prelude hiding (filter)

import qualified Data.Machine as Mc
import Data.Machine ((~>))
import qualified Control.Category as Cat
import Control.Monad (liftM, mzero, forever)
import Control.Arrow
import Control.Applicative
import Control.Monad.Trans
import Debug.Trace

import Control.Arrow.Machine.Types
import Control.Arrow.Machine.Event
import Control.Arrow.Machine.Detail

import qualified Control.Arrow.Machine.Plan as Pl

delay :: ArrowApply a => ProcessA a (Event b) (Event b)
delay = toProcessA delayImpl >>> arr (fromEvent NoEvent)
  where
    delayImpl = Mc.repeatedly $
      do
        x <- awaitA
        Mc.yield $ NoEvent
        Mc.yield $ Event x


{-
hold :: ArrowApply a => b -> ProcessA a (Event b) b
hold init = ProcessA $ holdImpl init


holdImpl :: ArrowApply a => b -> ProcessA_ a t (Maybe (Event b), d) -> ProcessA_ a  t (Maybe b, d)
holdImpl init (ProcessA_ pre post mc) = 
        ProcessA_ 
           (pre' pre) 
           (post' post) 
           (mc' pre init undefined mc)
  where
    -- pre' pre arg = (pre <$> ((,) <$> fst arg <*> Event (snd arg)) , ())
    pre' pre arg = 
        case arg 
           of
             (Just x, y) ->
                 (Event (x, y), snd $ pre (Just (evMaybe init id x), y))
             (Nothing, y) ->
                 (NoEvent, snd $ pre (Nothing, y))

    post' post (arg, r) = 
        evMaybe (post (NoEvent, r)) post arg -- 極めて怪しい

    mc' pre held _ (Mc.Stop) = 
        Mc.Stop
    mc' pre held r0 mc@(Mc.Await fc f ff) = 
        Mc.Await 
          id
          (proc (evx, d) ->
            do
              x <- arr (evMaybe held id) -< evx
              (evp, r) <- arr pre -< (Just x, d)
              (| hEv'
                  (\p -> do {y <- f -< p; returnA -< mc' pre x r (fc y)})
                  (returnA -< mc' pre held r0 mc)
                  (returnA -< Mc.Stop) --(returnA -< mc' pre held r0 mc)
                |)
                  evp)
          (mc' pre held r0 ff)
    mc' pre held r (Mc.Yield q fc) = 
        Mc.Yield (Event q, r) (mc' pre held r fc)
-}
sense :: (ArrowApply a, Eq b) =>
         ProcessA a b (Event b)
sense = arr Event >>> toProcessA (Mc.construct (mc Nothing))
  where
    mc (mayPrev) =
      do
        x <- awaitA
        let differs = maybe True (not.(x ==)) mayPrev

        if differs then Mc.yield x else return ()
        mc (Just x)

{-
once :: ArrowApply a =>
        a b c ->
        ProcessA a (Event b) (Maybe c)
once action = toProcessA go >>> hold Nothing
  where
    go = Mc.construct $
      do
        ret <- Mc.request action
        Mc.yield $ Just ret
        forever $ Mc.request (arr id)
-}


anyTime :: ArrowApply a =>
        a b c ->
        ProcessA a (Event b) (Event c)
anyTime action = toProcessA go
  where
    go = Mc.repeatedly $
      do
        ret <- Mc.request action
        Mc.yield ret


filter :: ArrowApply a =>
          a b Bool ->
          ProcessA a (Event b) (Event b)
filter cond = toProcessA $ Mc.repeatedly $
  do
    mayReturn <- Mc.request $ proc x ->
      do
        b <- cond -< x
        returnA -< if b then Just x else Nothing
    maybe (return ()) Mc.yield mayReturn


pass :: ArrowApply a =>
          ProcessA a (Event b) (Event b)
pass = filter (arr (const True))

{-
accumulate :: (ArrowApply a, ArrowLoop a) => 
              (c->b->c) -> c -> ProcessA a (Event b) c
accumulate f init = proc evx -> 
  do
    rec
      current <- hold init <<< delay -< next
      next <- returnA -< f current `fmap` evx
    returnA -< current
-}

--
-- ローカルBehavior
--
infixr 9 `passRecent`

passRecent :: (ArrowApply a, Occasional o) =>
              ProcessA a e (Event b) ->
              ProcessA a (e, b) o ->
              ProcessA a e o

passRecent af ag = proc e ->
  do
    new <- af -< e
    val <- toProcessA (Mc.construct $ corePlan NoEvent) -< Event (new, e)
    case val
      of
        Event b -> ag -< (e, b)
        NoEvent -> returnA -< noEvent
        End -> returnA -< end
  where
    corePlan evPrev = 
      do
        (evCur, _) <- awaitA
        let evNew = case evCur of {Event _ -> evCur; _ -> evPrev}
        evMaybe (return ()) Mc.yield evNew
        corePlan evNew

withRecent :: (ArrowApply a, Occasional o) =>
              ProcessA a (e, b) o ->
              ProcessA a (e, Event b) o
withRecent af = proc (e, evb) ->
    (returnA -< evb) `passRecent` (\b -> af -< (e, b))


variable :: (ArrowApply a, ArrowLoop a, Occasional o) =>
            ProcessA a e b ->
            ProcessA a (e, b) (o, Event b) ->
            ProcessA a e o

variable af ag = proc e ->
  do
    binit <- af -< e
    rec 
        (o, bnew) <-(| withRecent (\b -> ag -< (e, b)) |) bdly
        bdly <- (once -< Event binit) `successive` (delay -< bnew)
    returnA -< o

once :: ArrowApply a => ProcessA a (Event b) (Event b)
once = toProcessA $ Mc.construct $ awaitA >>= Mc.yield

successive :: ArrowApply a => 
              ProcessA a e (Event o) ->
              ProcessA a e (Event o) ->
              ProcessA a e (Event o)
successive af ag = proc e ->
  do
    x <- af -< e
    if isEnd x
      then
        ag -< e
      else
        returnA -< x

---
--- ステップ実行
---
type Running a b c = ProcessA_ a b c

startRun :: Arrow a =>
             ProcessA a (Event b) (Event c) ->
             ProcessA_ a (Event b) (Event c)
startRun = resolveCPS
stepRun :: ArrowApply a =>
            ProcessA_ a (Event b) (Event c) ->
            a b ([c], ProcessA_ a (Event b) (Event c))
stepRun (ProcessA_ pre post mc) = proc x ->
  do
    let
        (evp, r) = pre (Event x)
    (ret, mc') <- feedTo id mc -< evp
    let
        evcs = map (\q -> post (q, r)) ret
        cs = evcs >>= evMaybe mzero return
    returnA -< (cs, ProcessA_ pre post mc')



isStop :: ProcessA_ a b c -> Bool
isStop (ProcessA_ pre post Mc.Stop) = True
isStop _ = False

{-
        Mc.construct (holder pre x) ~> Mc.fit first mc
    holder pre xprev = 
      do
        (evx, d) <- Mc.await
        held <- case evx of 
          Event x -> x
          _ -> xprev
        Mc.yield $ pre (held, d)
        holder pre held
-}        
  
