{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module
    Types.BasicSpec
where

import qualified Control.Arrow.Machine as P

import Control.Arrow.Machine hiding (filter, source)
import Control.Applicative
import qualified Control.Category as Cat
import Control.Arrow
import Control.Monad.State
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity (Identity, runIdentity)
import Debug.Trace
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary, oneof, frequency, sized)

import Common.RandomProc


spec =
  do
    it "is stream transducer." $
      do
        let
          process = repeatedly $
            do
              x <- await
              yield x
              yield (x + 1)

          resultA = run process [1,2,4]

        resultA `shouldBe` [1, 2, 2, 3, 4, 5]

    let
        -- 入力1度につき同じ値を2回出力する
        doubler = repeatedly $
                  do {x <- await; yield x; yield x}
        -- 入力値をStateのリストの先頭にPushする副作用を行い、同じ値を出力する
        pusher = repeatedlyT $
                 do {x <- await; lift $ modify (x:); yield x}

    it "has stop state" $
      let
          -- 一度だけ入力をそのまま出力し、すぐに停止する
          onlyOnce = construct $ await >>= yield

          x = stateProc (doubler >>> pusher >>> onlyOnce) [3, 3]
        in
          -- 最後尾のMachineが停止した時点で処理を停止するが、
          -- 既にa2が出力した値の副作用は処理する
          x `shouldBe` ([3], [3, 3])

    it "has side-effect" $
      let
          incl = evMap (+1)

          -- doublerで信号が2つに分岐する。
          -- このとき、副作用は1つ目の信号について末尾まで
          -- -> 二つ目の信号について分岐点から末尾まで ...
          -- の順で処理される。
          a = pusher >>> doubler >>> incl >>> pusher >>> incl >>> pusher

          x = stateProc a [1000]
        in
          x `shouldBe` ([1002, 1002], reverse [1000,1001,1002,1001,1002])

    it "never spoils any FEED" $
      let
          counter = construct $ counterDo 1
          counterDo n =
            do
              x <- await
              yield $ n * 100 + x
              counterDo (n+1)
          x = stateProc (doubler >>> doubler >>> counter) [1,2]
        in
          fst x `shouldBe` [101, 201, 301, 401, 502, 602, 702, 802]

    prop "each path can have independent number of events" $ \l ->
      let
          split2' = fmap fst &&& fmap snd
          gen = arr (fmap $ \x -> [x, x]) >>> fork >>> arr split2'
          r1 = run (gen >>> arr fst) (l::[(Int, [Int])])
          r2 = run (gen >>> second (fork >>> repeatedly (await >>= yield)) >>> arr fst)
               (l::[(Int, [Int])])
        in
          r1 == r2

    it "is lazy for individual input values" $
      do
        let l = run Cat.id (take 10 $ repeat undefined)
        length l `shouldBe` 10

{-
    it "is lazy for inpurt stream" $
      do
        let l = take 10 $ run Cat.id (repeat undefined)
        length l `shouldBe` 10
-}
