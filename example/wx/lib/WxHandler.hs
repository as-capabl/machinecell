{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module
    WxHandler
where

import qualified Control.Arrow.Machine as P
import Control.Applicative ((<$>), (<*>))
import qualified Data.Machine as Mc
import qualified Control.Category as Cat
import Control.Arrow
import Control.Arrow.ArrowIO
import Control.Monad.State
import Control.Monad
import Debug.Trace

import qualified Graphics.UI.WX as Wx
import Graphics.UI.WX (Prop ((:=)))
import qualified Graphics.UI.WXCore as WxC




-- イベント処理
type MainState a = P.Running a
                    (P.Event (EventID, EventArg)) (P.Event ())

newtype EventID = EventID Int deriving (Eq, Show)
data EventEnv a = EventEnv {
      envGetIDPool :: Wx.Var EventID,
      envGetState :: Wx.Var (MainState a),
      envGetRun :: forall b c. a b c -> b -> IO c
    }

data EventArg = EventNoArg | 
                EventMouse Wx.EventMouse 
    deriving Show

data World a = World {
      worldGetEnv :: EventEnv a,
      worldGetEvent :: P.Event (EventID, EventArg)
}

eventHandler env eid arg =
  do
    stH <- Wx.varGet $ envGetState env
    (_, stH') <- envGetRun env (P.stepRun stH) (eid, arg)
    envGetState env `Wx.varSet` stH'



initialID = EventID 0
inclID (EventID n) = EventID (n+1)
newID env =  Wx.varUpdate (envGetIDPool env) inclID




wxReactimate :: (ArrowIO a, ArrowApply a) =>
                (forall b c. a b c -> b -> IO c) ->
                (P.ProcessA a
                    (World a) (P.Event ())) -> 
                IO ()
wxReactimate run init = Wx.start go
  where
    go =
      do
        rec vID <- Wx.varCreate $ inclID initialID
            vSt <- Wx.varCreate st

            let env = EventEnv { 
                      envGetIDPool = vID,
                      envGetState = vSt,
                      envGetRun = run
                    }

            let init' = proc etp -> init -< World env etp

            let st = P.startRun init'


        eventHandler env initialID EventNoArg




listen :: (ArrowIO a, ArrowApply a) =>
          a (EventArg -> IO (), initArg) () ->
          a EventArg ev ->
              P.ProcessA a
                   (World a, initArg)
                   (P.Event ev)
listen reg getter = proc (World env etp, ia) ->
  do
    P.toProcessA (Mc.construct go) -< 
       retuple <$> etp <*> P.Event env <*>P.Event ia
  where
    retuple (eid, ea) env ia = 
        (env, eid, ea, ia)

    go =
      do
        myID <- Mc.request $ proc (env, curID, _, ia) ->
          do
            returnA -< 
                if not (curID == initialID) 
                  then 
                    error "need initialization!" 
                  else
                    ()
            myID <- arrIO newID -< env
            reg -< (eventHandler env myID, ia)
            returnA -< myID
        
        forever $ 
          do
            mayReturn <- Mc.request $ proc (env, curID, ea, _) ->
              app -< 
                  if curID == myID
                    then
                      (arr Just <<< getter, ea)
                    else do
                      (arr (const Nothing), ea)
            maybe (return ()) Mc.yield mayReturn



onMouse :: (Wx.Reactive w, ArrowIO a, ArrowApply a) => 
         P.ProcessA a (World a, w) 
              (P.Event WxC.EventMouse)
onMouse = listen (arrIO2 regIO) (arr getter)
  where
    regIO handler w = Wx.set w [Wx.on Wx.mouse := (handler . EventMouse)]
                   
    getter (EventMouse x) = x
    getter _ = error "Event mismatch (internal error)"



onCommand :: (Wx.Commanding w, ArrowIO a, ArrowApply a) => 
         P.ProcessA a (World a, w) (P.Event ())
onCommand = listen (arrIO2 regIO) (arr getter)
  where
    regIO handler w = Wx.set w [Wx.on Wx.command := (handler EventNoArg)]

    getter = const ()
          


onInit = proc (World _ etp) -> af -< etp
  where
    af = P.construct $
      do
        ret <- P.awaits $ \(eid, _) ->
          do
            if not (eid == initialID)
              then
                error "Need initialization"
              else
                return ()
        P.yield ()
        forever $ P.awaitDo $ return ()
