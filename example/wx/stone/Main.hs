{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module
    Main
where

import qualified Control.Arrow.Machine as P
import Control.Applicative ((<$>), (<*>), (<$))
import qualified Control.Category as Cat
import Control.Arrow
import Control.Arrow.ArrowIO
import Control.Monad
import Control.Monad.Trans
import Control.Lens
import System.Random
import Debug.Trace

import qualified Graphics.UI.WX as Wx
import Graphics.UI.WX (Prop ((:=)))
import qualified Graphics.UI.WXCore as WxC

import qualified WxHandler as WxP


--
-- 定義
--

-- Mainアロー
type MainArrow = Kleisli IO
runMainArrow = runKleisli
instance ArrowIO MainArrow
  where
    arrIO = Kleisli


-- フォーム
data MyForm a = MyForm { 
      myFormF :: Wx.Frame a,
      myFormLabel :: Wx.StaticText a,
      myFormCounter :: Wx.StaticText a,
      myFormBtns :: [(Int, Wx.Button a)]
}


-- コマンド
data Command = NewGame | Message String | Stone Int
makePrisms ''Command

forkOf ::
    ArrowApply a =>
    Fold s b -> P.ProcessA a (P.Event s) (P.Event b)
forkOf fd = P.repeatedly $ P.await >>= mapMOf_ fd P.yield


--
-- 処理
--

-- ボタンリストのイベント待機
onBtnAll :: (ArrowApply a, ArrowIO a) =>
    [(b, Wx.Button c)] -> P.ProcessA a (WxP.World a) (P.Event b)
onBtnAll btns = 
    P.gather <<< P.parB (make <$> btns)
  where
    make (num, btn) = proc world -> 
      do
        ev <- WxP.on0 Wx.command -< (world, btn)
        returnA -< num <$ ev


-- 処理の本体
machine = proc world ->
  do
    initMsg <- WxP.onInit -< world
    form <- P.anytime (arrIO0 setup) -< initMsg

    -- formが作成されたらgoにスイッチ
    P.rSwitch (arr $ const P.noEvent) -< (world, go <$> form)

  where
    -- GUI初期化
    setup = 
      do
        f <- Wx.frame [Wx.text := "Take stones"]
        lbl <- Wx.staticText f [Wx.text := "A player who takes the last stone will lose."]
        cntr <- Wx.staticText f [Wx.text := "000"]

        btns <- forM [1, 2, 3] $ \i ->
          do
            btn <- Wx.button f [Wx.text := show i]
            return (i, btn)

        Wx.set f [Wx.layout := Wx.column 5 
                        ([Wx.widget lbl, Wx.widget cntr] ++ (Wx.widget <$> snd <$> btns))]

        return $ MyForm f lbl cntr btns

    -- メインの処理
    go MyForm{..} = proc world ->
      do
        rec
            -- ボタンから入力
            took <- onBtnAll myFormBtns -< world
    
            -- ゲームコルーチンを走らせる
            numStones' <- P.cycleDelay -< numStones
            command <- game myFormF -< (,) numStones' <$> took
    
            -- ゲーム開始をハンドル
            newGameMsg <- forkOf _NewGame -< command
            newGameStones <- P.anytime (arrIO0 $ randomRIO (7, 30)) -< newGameMsg
            
            -- 新しい石の数を適用
            newStones <- forkOf _Stone -< command
            numStones <- P.hold (-1) <<< P.gather -< [newStones, newGameStones]

        -- 数ラベル
        WxP.bind Wx.text -< (myFormCounter, show numStones)

        -- メッセージ
        message <- P.hold "" <<< forkOf _Message -< command
        WxP.bind Wx.text -< (myFormLabel, message)
        
        returnA -< P.noEvent


game f = P.constructT arrIO0 $
  do
    P.yield NewGame
    P.yield $ Message "A player who takes the last stone will lose."

    forever $
      do
        -- ボタン入力を待つ
        (n, youTook) <- P.await

        let 
            n' = n - youTook -- プレイヤーが取った後の石
            cpuTook' = (n' - 1) `mod` 4
            cpuTook = if cpuTook' == 0 then 1 else cpuTook' -- CPUが取る石
        P.yield $ Stone $ if n' <= 0 then n' else n' - cpuTook
        P.yield $ Message $
            "You took " ++ show youTook ++ 
                if n' > 0 then ", CPU took " ++ show cpuTook ++ "."
                          else "."

        -- ダイアログの表示(別にコルーチンの中でする必要はないが、デモとして)
        if
            | n' <= 0 ->
              do
                lift $ Wx.infoDialog f "Game over" "You lose."
                P.yield NewGame
                P.yield $ Message $ "New game."

            | n' - cpuTook <= 0 ->
              do
                lift $ Wx.infoDialog f "Game over" "You win."
                P.yield NewGame
                P.yield $ Message $ "New game."

            | otherwise ->
                return ()
            



main = 
  do
    WxP.wxReactimate runMainArrow machine
