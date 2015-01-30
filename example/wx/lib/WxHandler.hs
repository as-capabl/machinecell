{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module
    WxHandler 
      (
        World,
        -- * Event
        on,
        on0,
        onInit,
        
        -- * Property
        bind,
        
        -- * Running
        wxReactimate
      )
where

import qualified Control.Arrow.Machine as P
import qualified Control.Arrow.Machine.Misc.Discrete as D
import Data.Functor ((<$>), (<$))
import qualified Control.Category as Cat
import Control.Arrow
import Control.Arrow.ArrowIO
import Control.Monad

import Unsafe.Coerce

import qualified Graphics.UI.WX as Wx
import Graphics.UI.WX (Prop ((:=)))
import qualified Graphics.UI.WXCore as WxC


-- IORefのラップ
type MyRef a = Wx.Var a
newMyRef = Wx.varCreate
myRefGet = Wx.varGet
myRefSet = Wx.varSet


-- イベントID
newtype EventID = EventID Int deriving (Eq, Show)

initialID = EventID 0
inclID (EventID n) = EventID (n+1)
newID env =  Wx.varUpdate (envGetIDPool env) inclID


-- Internal data.
data Any

type MainState a = P.ProcessA a
                    (P.Event (EventID, Any)) (P.Event ())

data EventEnv a = EventEnv {
      envGetIDPool :: MyRef EventID,
      envGetState :: MyRef (MainState a),
      envGetRun :: forall b c. a b c -> b -> IO c
    }

data World a = World {
      worldGetEnv :: EventEnv a,
      worldGetEvent :: P.Event (EventID, Any)
}



-- Internal functions.
listenID :: 
    ArrowApply a =>
    P.ProcessA a (World a, EventID) (P.Event Any)
listenID = proc (World _ etp, myID) ->
  do
     ret <- P.filter (arr fst) -< go myID <$> etp
     returnA -< snd <$> ret

  where
    go myID (curID, ea) = (curID == myID, ea)


listen :: 
    (ArrowIO a, ArrowApply a, Eq w) =>
    a (Any -> IO (), w) () ->
    a Any ev -> 
    P.ProcessA a (World a, w) (P.Event ev)
listen reg getter = proc (world@(World env etp), ia) ->
  do
    initMsg <- P.edge -< ia
    evId <- P.anytime (arrIO newID) -< env <$ initMsg

    (returnA -< evId) `P.passRecent` \myID ->
      do
        P.anytime reg -< (handleProc env myID, ia) <$ initMsg
     
        ea <- listenID -< (world, myID)
        P.anytime getter -< ea


handleProc env eid arg =
  do
    stH <- myRefGet $ envGetState env
    (_, stH') <- envGetRun env (P.stepRun stH) (eid, arg)
    envGetState env `myRefSet` stH'


-- |Fires once on initialization.
onInit :: 
    (ArrowApply a) => 
    P.ProcessA a (World a) (P.Event ())
onInit = proc world -> 
  do
    ea <- listenID -< (world, initialID)
    P.echo -< () <$ ea


-- |Fires on Wx event.
on :: 
    (Eq w, ArrowIO a, ArrowApply a) => 
    Wx.Event w (arg -> IO ()) -> 
    P.ProcessA a (World a, w) (P.Event arg)
on signal = listen (arrIO2 regIO) (arr getter)
  where
    regIO handler w = 
        Wx.set w [Wx.on signal := (handler . unsafeCoerce)]
    getter = arr unsafeCoerce


-- |No argument version of `on`.
on0 :: 
    (ArrowIO a, Arrow a, ArrowApply a, Eq w) =>
    Wx.Event w (IO ()) -> 
    P.ProcessA a (World a, w) (P.Event ())
on0 signal = listen (arrIO2 regIO) (arr getter)
  where
    regIO handler w = 
        Wx.set w [Wx.on signal := handler (unsafeCoerce ())]
    getter = const ()

-- |Bind a behaviour to an attribute.
bind ::
    (ArrowIO a, Arrow a, ArrowApply a, Eq w) =>
    Wx.Attr w b ->
    P.ProcessA a (w, D.T b) ()
bind attr = proc (w, x) ->
  do
    wd <- D.fromEq -< w
    ev <- D.edge <<< D.arr2 (,) -< (wd, x)
    P.anytime
        (arrIO (\(w, val) -> Wx.set w [attr := val]))
            -< ev
    returnA -< ()

-- |Actuate an event handling process.
wxReactimate :: 
    (ArrowIO a, ArrowApply a) =>
    (forall b c. a b c -> b -> IO c) ->
    P.ProcessA a (World a) (P.Event ()) -> 
    IO ()
wxReactimate run init = Wx.start go
  where
    go =
      do
        rec vID <- newMyRef $ inclID initialID
            vSt <- newMyRef st

            let env = EventEnv { 
                      envGetIDPool = vID,
                      envGetState = vSt,
                      envGetRun = run
                    }

            let init' = proc etp -> init -< World env etp

            let st = init'


        handleProc env initialID (unsafeCoerce ())
