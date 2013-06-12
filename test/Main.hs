{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module
    Main
where

import Control.Arrow.Machine
import Control.Applicative ((<$>))
import qualified Data.Machine as Mc
import qualified Control.Category as Cat
import Control.Arrow
import Control.Monad.State
import Control.Monad
import Control.Monad.Trans
import Debug.Trace
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary, oneof, frequency, sized)
import RandomProc


myProc2 :: ProcessA (Kleisli (State [Int])) (Event Int) (Event Int)
myProc2 = toProcessA $ Mc.repeatedly core
  where
    core = 
      do
        z <- Mc.request (Kleisli $ \x -> do {modify (++ [x]); return x})
        Mc.yield `mapM` (take z $ repeat z)


-- helper
toN (Event x) = Just x
toN NoEvent = Nothing
en (ex, ey) = Event (toN ex, toN ey)
de evxy = (fst <$> evxy, snd <$> evxy)



main = hspec $
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
            result `shouldBe` (map (\x->(Just x, Just x)) [1,2,2,3,3,3])
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
              de (Event ((x, y),z))= ((Event x, Event y), Event z)
              de _ = ((NoEvent, NoEvent), NoEvent)
              
              assoc ((a,b),c) = (a,(b,c))

              x1 = stateProc (arr de >>> first (first f) >>> arr assoc >>> arr en) (l::[((Int, Int), Int)])
              x2 = stateProc (arr de >>> arr assoc >>> first f >>> arr en) (l::[((Int, Int), Int)])
            in
              x1 == x2
{-
    describe "ProcessA as ArrowChoice" $
      do        
        it "can use if in proc" $
          do
            let f = proc evx ->
              do
                x <- (|hEv (\x -> returnA -< ) ()
                if 
-}
