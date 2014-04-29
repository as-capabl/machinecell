{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module
    Main
where

import Data.Maybe (fromMaybe)
import Control.Arrow.Machine as P
import Control.Applicative ((<$>), (<*>), (<$))
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
import RandomProc

runKI a x = runIdentity (runKleisli a x)




main = hspec $ do {basics; rules; loops; choice; plans; utility; switches; execution}


basics =
  do
    describe "ProcessA" $
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
            pusher = repeatedlyT (Kleisli . const) $
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
              incl = arr $ fmap (+1)

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
              split2' (Event (x, y)) = (Event x, Event y)
              split2' NoEvent = (NoEvent, NoEvent)
              split2' End = (End, End)
              gen = arr (fmap $ \x -> [x, x]) >>> fork >>> arr split2'
              r1 = runKI (run (gen >>> arr fst)) (l::[(Int, [Int])])
              r2 = runKI (run (gen >>> second (fork >>> echo) >>> arr fst)) 
                   (l::[(Int, [Int])])
            in
              r1 == r2


rules =
  do
    describe "ProcessA as Category" $
      do        
        prop "has asocciative composition" $ \fx gx hx cond ->
          let
              f = mkProc fx
              g = mkProc gx
              h = mkProc hx
              equiv = mkEquivTest cond
            in
              ((f >>> g) >>> h) `equiv` (f >>> (g >>> h))

        prop "has identity" $ \fx gx cond ->
          let
              f = mkProc fx
              g = mkProc gx
              equiv = mkEquivTest cond
            in
              (f >>> g) `equiv` (f >>> Cat.id >>> g)

    describe "ProcessA as Arrow" $
      do        
        it "can be made from pure function(arr)" $
          do
            (run . arr . fmap $ (+ 2)) [1, 2, 3]
              `shouldBe` [3, 4, 5]

        prop "arr id is identity" $ \fx gx cond ->
          let
              f = mkProc fx
              g = mkProc gx
              equiv = mkEquivTest cond
            in
              (f >>> g) `equiv` (f >>> arr id >>> g)

        it "can be parallelized" $
          let
            in
          do
            let 
                myProc2 = repeatedlyT (Kleisli . const) $
                  do
                    x <- await
                    lift $ modify (++ [x])
                    yield `mapM` (take x $ repeat x)

                toN (Event x) = Just x
                toN NoEvent = Nothing
                toN End = Nothing
                en (ex, ey) = Event (toN ex, toN ey)
                de evxy = (fst <$> evxy, snd <$> evxy)

                l = map (\x->(x,x)) [1,2,3]

                (result, state) =
                    stateProc (arr de >>> first myProc2 >>> arr en) l
                                  
            (result >>= maybe mzero return . fst) 
                `shouldBe` [1,2,2,3,3,3]
            (result >>= maybe mzero return . snd) 
                `shouldBe` [1,2,3]
            state `shouldBe` [1,2,3]

        prop "first and composition." $ \fx gx cond ->
          let
              f = mkProc fx
              g = mkProc gx
              equiv = mkEquivTest2 cond
            in
              (first (f >>> g)) `equiv` (first f >>> first g)

        prop "first-second commutes" $  \fx cond ->
          let
              f = first $ mkProc fx
              g = second (arr $ fmap (+2))
              
              equiv = mkEquivTest2 cond
            in
              (f >>> g) `equiv` (g >>> f)

        prop "first-fst commutes" $  \fx cond ->
          let
              f = mkProc fx
              equiv = mkEquivTest cond
                    ::(MyTestT (Event Int, Event Int) (Event Int))
            in
              (first f >>> arr fst) `equiv` (arr fst >>> f)

        prop "assoc relation" $ \fx cond ->
          let
              f = mkProc fx
              assoc ((a,b),c) = (a,(b,c))

              equiv = mkEquivTest cond
                    ::(MyTestT ((Event Int, Event Int), Event Int)
                               (Event Int, (Event Int, Event Int)))
            in
              (first (first f) >>> arr assoc) `equiv` (arr assoc >>> first f)

loops =
  do
    describe "ProcessA as ArrowLoop" $
      do
        it "can be used with rec statement(pure)" $
          let
              a = proc x ->
                do
                  rec l <- returnA -< evMaybe [] (:l) x
                  returnA -< l <$ x
              result = fst $ stateProc a [2, 5]
            in
              take 3 (result!!1) `shouldBe` [5, 5, 5]

        it "can be used with rec statement(macninery)" $
          let
              mc = anytime Cat.id
              a = proc x ->
                do
                  rec l <- mc -< (:l') <$> x
                      l' <- returnA -< fromEvent [] l
                  returnA -< l
              result = fst $ stateProc a [2, 5]
            in
              take 3 (result!!1) `shouldBe` [5, 5, 5]

        it "the last value is valid." $
          do
            let
                mc = repeatedly $
                  do
                    x <- await
                    yield x
                    yield (x*2)
                pa = proc x ->
                  do
                    rec y <- mc -< (+z) <$> x
                        z <- hold 0 <<< delay -< y
                    returnA -< y
            run pa [1, 10] `shouldBe` [1, 2, 12, 24]

    describe "Rules for ArrowLoop" $
      do
        let
            fixcore f y = if y `mod` 5 == 0 then y else y + f (y-1)
            pure (evx, f) = (f <$> evx, fixcore f)
            apure = arr pure

        prop "left tightening" $ \fx cond ->
          let
              f = mkProc fx

              equiv = mkEquivTest cond
            in
              (loop (first f >>> apure)) `equiv` (f >>> loop apure)

        it "right tigntening"
           pending
{-
        prop "right tightening" $ \fx cond ->
          let
              f = mkProc fx

              equiv = mkEquivTest cond
            in
              (loop (apure >>> first f)) `equiv` (loop apure >>> f)
-}

choice =
  do
    describe "ProcessA as ArrowChoice" $
      do
        it "temp1" $
         do
           let
                af = mkProc $ PgStop
                ag = mkProc $ PgOdd PgNop
                aj1 = arr Right
                aj2 = arr $ either id id
                l = [1]
                r1 = stateProc 
                       (aj1 >>> left af >>> aj2) 
                       l
              in
                r1 `shouldBe` ([1],[])

        prop "left (f >>> g) = left f >>> left g" $ \fx gx cond ->
            let
                f = mkProc fx
                g = mkProc gx
                
                equiv = mkEquivTest cond
                    ::(MyTestT (Either (Event Int) (Event Int))
                               (Either (Event Int) (Event Int)))
              in
                (left (f >>> g)) `equiv` (left f >>> left g)


plans = describe "Plan" $
  do
    let pl = 
          do
            x <- await
            yield x
            yield (x+1)
            x <- await
            yield x
            yield (x+1)
        l = [2, 5, 10, 20, 100]

    it "can be constructed into ProcessA" $
      do
        let 
            result = run (construct pl) l
        result `shouldBe` [2, 3, 5, 6]

    it "can be repeatedly constructed into ProcessA" $
      do
        let
            result = run (repeatedly pl) l
        result `shouldBe` [2, 3, 5, 6, 10, 11, 20, 21, 100, 101]


utility =
  do
    describe "delay" $
      do
        it "delays input" $
          do
            run (arr (\x->(x,x)) >>> first delay >>> arr fst) [0, 1, 2] `shouldBe` [0, 1, 2]
            run (arr (\x->(x,x)) >>> first delay >>> arr snd) [0, 1, 2] `shouldBe` [0, 1, 2]


switches =
  do
    describe "switch" $
      do
        it "switches once" $
          do
            let 
                before = proc evx -> 
                  do
                    ch <- P.filter (arr $ (\x -> x `mod` 2 == 0)) -< evx
                    returnA -< (NoEvent, ch)

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
                   (evx, evarr) <- P.split -< evtp
                   sw (arr $ fmap (+2)) -< (evx, evarr)

               l = [(Event 5, NoEvent),
                    (Event 1, Event (arr $ fmap (*2))),
                    (Event 2, NoEvent)]
               ret = run (theArrow rSwitch) l
               retD = run (theArrow drSwitch) l

            ret `shouldBe` [7, 2, 4]
            retD `shouldBe` [7, 3, 4]

execution = describe "Execution of ProcessA" $
    do
      let
          pl = 
            do
              x <- await
              yield x
              yield (x+1)
              x <- await
              yield x
              yield (x+1)
              yield (x+5)
          init = construct pl

      it "supports step execution" $
        do
          let
              (ret, now) = stepRun init 1
          yields ret `shouldBe` [1, 2]
          hasStopped ret `shouldBe` False

          let
              (ret, now2) = stepRun now 1
          yields ret `shouldBe` [1, 2, 6]
          hasStopped ret `shouldBe` True

          let
              (ret, _) = stepRun now2 1
          yields ret `shouldBe` ([]::[Int])
          hasStopped ret `shouldBe` True

      it "supports yield-driven step" $
        do
          let
              init = construct $ 
                do
                  yield (-1)
                  x <- await
                  mapM yield (iterate (+1) x) -- infinite

              (ret, now) = stepYield init 5
          yields ret `shouldBe` Just (-1)
          hasConsumed ret `shouldBe` False
          hasStopped ret `shouldBe` False

          let
              (ret, now2) = stepYield now 10
          yields ret `shouldBe` Just 10
          hasConsumed ret `shouldBe` True
          hasStopped ret `shouldBe` False

          let
              (ret, now3) = stepYield now2 10
          yields ret `shouldBe` Just 11
          hasConsumed ret `shouldBe` False
          hasStopped ret `shouldBe` False

