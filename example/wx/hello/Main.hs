{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module
    Main
where

import qualified Control.Arrow.Machine as P
import Control.Applicative ((<$>), (<*>))
import qualified Control.Category as Cat
import Control.Arrow
import Control.Arrow.ArrowIO
import Control.Monad
import Debug.Trace

import qualified Graphics.UI.WX as Wx
import Graphics.UI.WX (Prop ((:=)))
import qualified Graphics.UI.WXCore as WxC

import WxHandler

type MainArrow = Kleisli IO
runMainArrow = runKleisli
instance ArrowIO MainArrow
  where
    arrIO = Kleisli


data MyForm a b = MyForm { 
      myFormF :: Wx.Frame a,
      myFormBtn :: Wx.Button b
}
machine = proc world ->
  do
    initMsg <- onInit -< world
    form <- P.once (arrIO0 setup) -< initMsg

    case form 
      of
        Nothing -> 
            returnA -< P.NoEvent

        Just (MyForm f btn) ->
          do    
            quitMsg <- onCommand -< (world, btn)
            P.anyTime (arrIO Wx.close) -< const f `fmap` quitMsg

            returnA -< P.NoEvent

  where
    setup = 
      do
        f <- Wx.frame [Wx.text := "Hello!"]
        btn <- Wx.button f [Wx.text := "Quit"]
        Wx.set f [Wx.layout := Wx.widget btn]
        return $ MyForm f btn


main = 
  do
    wxReactimate runMainArrow machine
