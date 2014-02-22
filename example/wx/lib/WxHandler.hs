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


-- イベントID
newtype EventID = EventID Int deriving (Eq, Show)

initialID = EventID 0
inclID (EventID n) = EventID (n+1)
newID env =  Wx.varUpdate (envGetIDPool env) inclID


-- 実行環境
type MainState a = P.Running a
                    (P.Event (EventID, EventArg)) (P.Event ())

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



-- イベントハンドリング
listenID :: ArrowApply a =>
            P.ProcessA a
                 (World a, EventID)
                 (P.Event EventArg)
listenID = proc (World _ etp, myID) ->
  do
    isEq <- P.filter (arr id) -< (\(eid, _) -> eid == myID) <$> etp
    returnA -< (\(_, ea) _ -> ea) <$> etp <*> isEq



onInit :: (ArrowApply a) => 
         P.ProcessA a (World a) (P.Event ())
onInit = proc world -> 
  do
    ea <- listenID -< (world,initialID)
    returnA -< const () `fmap` ea



listen :: (ArrowIO a, ArrowApply a) =>
          a (EventArg -> IO (), initArg) () ->
          a EventArg ev ->
              P.ProcessA a
                   (World a, initArg)
                   (P.Event ev)
listen reg getter = proc (world@(World env etp), ia) ->
  do
    initMsg <- onInit -< world
    mMyID <- P.once (arrIO newID) -< fmap (const env) initMsg

    case mMyID
      of
        Nothing -> 
            returnA -< P.NoEvent

        Just myID ->
          do
            P.once reg -< fmap (const (handleProc env myID, ia)) initMsg

            ea <- listenID -< (world, myID)
            P.anyTime getter -< ea


handleProc env eid arg =
  do
    stH <- Wx.varGet $ envGetState env
    (_, stH') <- envGetRun env (P.stepRun stH) (eid, arg)
    envGetState env `Wx.varSet` stH'


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


        handleProc env initialID EventNoArg


-- 個別のイベント(THで自動生成したい)
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
          

