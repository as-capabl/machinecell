{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module
    TempSpec
where

import Data.Maybe (fromMaybe)
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


runKI fmy x = runIdentity (fmy x)

spec =
  do
    utility
    switches
    source



utility =
  do
    describe "splitEvent" $
      do
        it "splits an event stream" $
          do
            run (splitEvent >>> arr fst) [Left 1, Right 2, Left 3, Right 4] `shouldBe` [1, 3]
            run (splitEvent >>> arr snd) [Left 1, Right 2, Left 3, Right 4] `shouldBe` [2, 4]

    describe "edge" $
      do
        it "detects edges of input behaviour" $
          do
            run (hold 0 >>> edge) [1, 1, 2, 2, 2, 3] `shouldBe` [0, 1, 2, 3]
            run (hold 0 >>> edge) [0, 1, 1, 2, 2, 2, 3] `shouldBe` [0, 1, 2, 3]

    describe "accum" $
      do
        it "acts like fold." $
          do
            let
                pa = proc evx ->
                  do
                    val <- accum 0 -< (+1) <$ evx
                    returnA -< val <$ evx

            run pa (replicate 10 ()) `shouldBe` [1..10]

    describe "onEnd" $
      do
        it "fires only once at the end of a stream." $
          do
            let
                pa = proc evx ->
                  do
                    x <- hold 0 -< evx
                    ed <- onEnd -< evx
                    returnA -< x <$ ed
            run pa [1..4] `shouldBe` [4]

    describe "gather" $
      do
        it "correctly handles the end" $
          do
            let
                pa = proc x ->
                  do
                    r1 <- P.filterEvent (\x -> x `mod` 3 == 0) -< x :: Event Int
                    r2 <- stopped -< x
                    r3 <- returnA -< r2
                    fin <- gather -< [r1, r2, r3]
                    val <- hold 0 -< r1
                    end <- onEnd -< fin
                    returnA -< val <$ end
            run pa [1, 2, 3, 4, 5] `shouldBe` ([3]::[Int])


switches =
  do
    describe "switch" $
      do
        it "switches once" $
          do
            let
                before = proc evx ->
                  do
                    ch <- P.filterEvent (\x -> x `mod` 2 == 0) -< evx
                    returnA -< (noEvent, ch)

                after t = proc evx -> returnA -< (t*) <$> evx

                l = [1,3,4,1,3,2]

                -- 最初に偶数が与えられるまでは、入力を無視(NoEvent)し、
                -- それ以降は最初に与えられた偶数 * 入力値を返す
                ret = run (switch before after) l

                -- dが付くと次回からの切り替えとなる
                retD = run (dSwitch before after) l

            ret `shouldBe` [16, 4, 12, 8]
            retD `shouldBe` [4, 12, 8]

    describe "rSwitch" $
      do
        it "switches any times" $
          do
            let
               theArrow sw = proc evtp ->
                 do
                   evx <- P.fork -< fst <$> evtp
                   evarr <- P.fork -< snd <$> evtp
                   sw (arr $ fmap (+2)) -< (evx, evarr)

               l = [(Just 5, Nothing),
                    (Just 1, Just (arr $ fmap (*2))),
                    (Just 3, Nothing),
                    (Just 6, Just (arr $ fmap (*3))),
                    (Just 7, Nothing)]
               ret = run (theArrow rSwitch) l
               retD = run (theArrow drSwitch) l

            ret `shouldBe` [7, 2, 6, 18, 21]
            retD `shouldBe` [7, 3, 6, 12, 21]
    describe "kSwitch" $
      do
        it "switches spontaneously" $
          do
            let
                oneshot x = pure () >>> blockingSource [x]
                theArrow sw = sw (oneshot False) (arr snd) $ \_ _ -> oneshot True
            run (theArrow kSwitch) [] `shouldBe` [True]
            run (theArrow dkSwitch) [] `shouldBe` [False, True]

source =
  do
    describe "source" $
      do
        it "provides interleaved source stream" $
          do
            let
                pa = proc cl ->
                  do
                    s1 <- P.source [1, 2, 3] -< cl
                    s2 <- P.source [4, 5, 6] -< cl
                    P.gather -< [s1, s2]
            P.run pa (repeat ()) `shouldBe` [1, 4, 2, 5, 3, 6]
    describe "blockingSource" $
      do
        it "provides blocking source stream" $
          do
            let
                pa = proc _ ->
                  do
                    s1 <- P.blockingSource [1, 2, 3] -< ()
                    s2 <- P.blockingSource [4, 5, 6] -< ()
                    P.gather -< [s1, s2]
            P.run pa (repeat ()) `shouldBe` [4, 5, 6, 1, 2, 3]

    describe "source and blockingSource" $
      do
        prop "[interleave blockingSource = source]" $ \l cond ->
            let
                _ = l::[Int]
                equiv = mkEquivTest cond
                    ::(MyTestT (Event Int) (Event Int))
              in
                P.source l `equiv` P.interleave (P.blockingSource l)

        prop "[blocking source = blockingSource]" $ \l cond ->
            let
                _ = l::[Int]
                equiv = mkEquivTest cond
                    ::(MyTestT (Event Int) (Event Int))
              in
                (pure () >>> P.blockingSource l)
                    `equiv` (pure () >>> P.blocking (P.source l))


