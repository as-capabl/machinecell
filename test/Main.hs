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
import Control.Arrow.Machine
import Control.Applicative ((<$>), (<*>), (<$))
import qualified Data.Machine as Mc
import Data.Machine ((<~))
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


myProc2 :: ProcessA (Kleisli (State [Int])) (Event Int) (Event Int)
myProc2 = toProcessA $ Mc.repeatedly core
  where
    action x = 
      do 
        modify (++ [x])
        return x
    core = 
      do
        z <- Mc.request $ Kleisli action
        Mc.yield `mapM` (take z $ repeat z)


-- helper
toN (Event x) = Just x
toN NoEvent = Nothing
toN End = Nothing
en (ex, ey) = Event (toN ex, toN ey)
de evxy = (fst <$> evxy, snd <$> evxy)

runKI a x = runIdentity (runKleisli a x)


main = hspec $ do {basics; rules; loops; choice; plans; utility; execution}

basics =
  do
    describe "ProcessA" $
      do
        it "is wrapper of Ekmett's Machine." $
          do
            let
              plan = 
                do
                  x <- Mc.await
                  Mc.yield x
                  Mc.yield (x + 1)
              process = Mc.repeatedly plan

            let
              l = [1,2,4,8,10]

            let
              -- Machineによる書き方 (runがMonadPlusを要求するのでMaybe)
              result = fromMaybe [] $ 
                Mc.run (Mc.supply l process <~ mzero)

              -- ProcessAによる等価な書き方
              resultA = runProcessA (toProcessA process) l

            resultA `shouldBe` result


        it "has stop state" $
          let
              -- 入力1度につき同じ値を2回出力する
              a2 = mkProc $ PgDouble PgNop
              -- 入力値をStateのリストの先頭にPushする副作用を行い、同じ値を出力する
              a3 = mkProc $ PgPush PgNop
              -- 一度だけ入力をそのまま出力し、すぐに停止する
              a4 = toProcessA $ Mc.construct $ awaitA >>= Mc.yield

              l = [3, 3]

              x = stateProc (a2 >>> a3 >>> a4)
                   (l::[Int])
            in
              -- 最後尾のMachineが停止した時点で処理を停止するが、
              -- 既にa2が出力した値の副作用は処理する
              x `shouldBe` ([3], [3, 3])

        it "has side-effect" $
          let
              l = [1000]
              doubler = mkProc $ PgDouble PgNop
              pusher = mkProc $ PgPush PgNop
              a = pusher >>> doubler >>> (arr $ fmap (+1)) >>> 
                  pusher >>> (arr $ fmap (+1)) >>> pusher
              x = stateProc a l
            in
              x `shouldBe` ([1002, 1002], reverse [1000,1001,1002,1001,1002])

        it "never spoils any FEED" $
          let
              doubler = mkProc $ PgDouble PgNop
              counter = toProcessA $ Mc.construct $ counterDo 1
              counterDo n = 
                do
                  x <- awaitA
                  Mc.yield $ n * 100 + x
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
              r1 = runKI (runProcessA (gen >>> arr fst)) (l::[(Int, [Int])])
              r2 = runKI (runProcessA (gen >>> second (fork >>> pass) >>> arr fst)) 
                   (l::[(Int, [Int])])
            in
              r1 == r2


rules =
  do
    describe "ProcessA as Category" $
      do        
        prop "has asocciative composition" $ \f g h l ->
          let
              af = mkProc f
              ag = mkProc g
              ah = mkProc h
              r1 = stateProc ((af >>> ag) >>> ah) l
              r2 = stateProc (af >>> (ag >>> ah)) l
            in
              r1 == r2
{-
1) ProcessA as Category has asocciative composition
Falsifiable (after 28 tests and 7 shrinks): 
PgDouble (PgDouble (PgPush (PgDouble (PgPop (PgNop,PgPop (PgStop,PgIncl (PgIncl PgNop)) (PjFst PgStop)) (PjFst (PgOdd (PgDouble PgNop)))))))
PgPop (PgPop (PgDouble (PgPush (PgDouble PgStop)),PgIncl PgNop) (PjSnd PgNop),PgNop) (PjFst (PgPop (PgNop,PgNop) (PjSnd PgNop)))
PgPop (PgNop,PgNop) (PjFst PgNop)
[0]
-}
        prop "has identity" $ \f g l ->
          let
              af = mkProc f
              ag = mkProc g
              r1 = stateProc (af >>> ag) l
              r2 = stateProc (af >>> Cat.id >>> ag) l
            in
              r1 == r2

    describe "ProcessA as Arrow" $
      do        
        it "can be made from pure function(arr)" $
          do
            (runProcessA . arr . fmap $ (+ 2)) [1, 2, 3]
              `shouldBe` [3, 4, 5]

        prop "arr id is identity" $ \f g l ->
          let
              af = mkProc f
              ag = mkProc g
              r1 = stateProc (af >>> ag) l
              r2 = stateProc (af >>> arr id >>> ag) l
            in
              r1 == r2

        it "can be parallelized" $
          let
            in
          do
            let (result, state) =
                    stateProc (arr de >>> first myProc2 >>> arr en) $ 
                                  map (\x->(x,x)) [1,2,3]
            (result >>= maybe mzero return . fst) 
                `shouldBe` [1,2,2,3,3,3]
            (result >>= maybe mzero return . snd) 
                `shouldBe` [1,2,3]
            state `shouldBe` [1,2,3]

        prop "first and composition." $ \(fx, gx, l) ->
          let
              f = mkProc fx
              g = mkProc gx
              x1 = stateProc (arr de >>> first (f >>> g) >>> arr en) (l::[(Int, Int)])
              x2 = stateProc (arr de >>> first f >>> first g >>> arr en) (l::[(Int, Int)])
            in
              x1 == x2

        prop "first-second commutes" $  \(fx, l) ->
          let
              a1 = first $ mkProc fx
              a2 = second (arr $ fmap (+2))

              x1 = stateProc (arr de >>> a1 >>> a2 >>> arr en)
                   (l::[(Int, Int)])
              x2 = stateProc (arr de >>> a2 >>> a1 >>> arr en)
                   (l::[(Int, Int)])
            in
              x1 == x2

        prop "first-fst commutes" $  \(fx, l) ->
          let
              a = mkProc fx

              x1 = stateProc (arr de >>> first a >>> arr fst)
                   (l::[(Int, Int)])
              x2 = stateProc (arr de >>> arr fst >>> a)
                   (l::[(Int, Int)])
            in
              x1 == x2

        prop "assoc relation" $ \(fx, l) ->
          let
              f = mkProc fx

              en (ex, (ey, ez)) = Event (toN ex, (toN ey, toN ez))
              toN (Event x) = Just x
              toN NoEvent = Nothing
              toN End = Nothing
              de (Event ((x, y),z))= ((Event x, Event y), Event z)
              de _ = ((NoEvent, NoEvent), NoEvent)
              
              assoc ((a,b),c) = (a,(b,c))

              x1 = stateProc (arr de >>> first (first f) >>> arr assoc >>> arr en) (l::[((Int, Int), Int)])
              x2 = stateProc (arr de >>> arr assoc >>> first f >>> arr en) (l::[((Int, Int), Int)])
            in
              x1 == x2

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
              mc = Mc.pass Cat.id
              a = proc x ->
                do
                  rec l <- toProcessA mc -< (:l') <$> x
                      l' <- returnA -< fromEvent [] l
                  returnA -< l
              result = fst $ stateProc a [2, 5]
            in
              take 3 (result!!1) `shouldBe` [5, 5, 5]

        it "the last value is valid." $
          do
            let
                mc = Mc.repeatedly $
                  do
                    x <- awaitA
                    Mc.yield x
                    Mc.yield (x*2)
                pa = proc x ->
                  do
                    rec y <- toProcessA mc -< (+z) <$> x
                        z <- hold 0 <<< delay -< y
                    returnA -< y
            runProcessA pa [1, 10] `shouldBe` [1, 2, 12, 24]

    describe "Rules for ArrowLoop" $
      do
        let
            fixcore f y = if y `mod` 5 == 0 then y else y + f (y-1)
            pure (evx, f) = (f <$> evx, fixcore f)
            apure = arr pure

        prop "left tightening" $ \(l, fx, fy, fz) ->
          let
              a1 = mkProc fx
              a2 = mkProc fy
              a3 = mkProc fz

              x1 = stateProc (a1 >>> loop (first a2 >>> apure) >>> a3)
                   (l::[Int])
              x2 = stateProc (a1 >>> a2 >>> loop apure >>> a3)
                   (l::[Int])
            in
              x1 == x2

        prop "right tightening" $ \(l, fx, fy, fz) ->
          let
              a1 = mkProc fx
              a2 = mkProc fy
              a3 = mkProc fz


              x1 = stateProc (a1 >>> loop (apure >>> first a2) >>> a3)
                   (l::[Int])
              x2 = stateProc (a1 >>> loop apure >>> a2 >>> a3)
                   (l::[Int])
            in
              x1 == x2

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

        prop "left (f >>> g) = left f >>> left g" $ \e f g h j1 l ->
            let
                ae = mkProc e
                af = mkProc f
                ag = mkProc g
                ah = mkProc h
                aj1 = arr $ if j1 then Left else Right
                aj2 = arr $ either id id
                r1 = stateProc 
                       (ae >>> aj1 >>> left (af >>> ag) >>> aj2 >>> ah) 
                       l
                r2 = stateProc 
                       (ae >>> aj1 >>> left af >>> left ag >>> aj2 >>> ah) 
                       l
              in
                r1 == r2


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
            result = runKI
                       (runProcessA (construct pl))
                       l
        result `shouldBe` [2, 3, 5, 6]

    it "can be repeatedly constructed into ProcessA" $
      do
        let
            result = runKI
                       (runProcessA (repeatedly pl))
                       l
        result `shouldBe` [2, 3, 5, 6, 10, 11, 20, 21, 100, 101]


utility =
  do
    describe "delay" $
      do
        it "delays input" $
          do
            runProcessA (arr (\x->(x,x)) >>> first delay >>> arr fst) [0, 1, 2] `shouldBe` [0, 1, 2]
            runProcessA (arr (\x->(x,x)) >>> first delay >>> arr snd) [0, 1, 2] `shouldBe` [0, 1, 2]



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
          init = startRun $ construct pl

      it "supports step execution" $
        do
          let
              (x, now) = runKI (stepRun init) 1
          x `shouldBe` [1, 2]

          let
              (x, now2) = runKI (stepRun now) 1
          x `shouldBe` [1, 2, 6]

          let
              (x, _) = runKI (stepRun now2) 1
          x `shouldBe` ([]::[Int])


