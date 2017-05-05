{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module
Types.RuleSpec
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
    describe "ProcessA as Category" $ catSpec
    describe "ProcessA as Arrow" $ arrSpec
    describe "Rules for ArrowLoop" $ arrowLoopSpec

catSpec =
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

arrSpec =
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
      do
        pendingWith "to correct"
{-
        let
            myProc2 = repeatedlyT (Kleisli . const) $
              do
                x <- await
                lift $ modify (++ [x])
                yield `mapM` (take x $ repeat x)

            toN = evMaybe Nothing Just
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
-}

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

arrowLoopSpec =
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

    prop "right tightening" $ \fx cond ->
      let
          f = mkProc fx

          equiv = mkEquivTest cond
        in
          (loop (apure >>> first f)) `equiv` (loop apure >>> f)
