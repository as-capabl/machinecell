{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module
    Types.PlanSpec
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

    it "can handle the end with catchP." $
      do
        let
            plCatch =
              do
                x <- await `catchP` (yield 1 >> stop)
                yield x
                y <- (yield 2 >> await >> yield 3 >> await) `catchP` (yield 4 >> return 5)
                yield y
                y <- (await >>= yield >> stop) `catchP` (yield 6 >> return 7)
                yield y
        run (construct plCatch) [] `shouldBe` [1]
        run (construct plCatch) [100] `shouldBe` [100, 2, 4, 5, 6, 7]
        run (construct plCatch) [100, 200] `shouldBe` [100, 2, 3, 4, 5, 6, 7]
        run (construct plCatch) [100, 200, 300] `shouldBe` [100, 2, 3, 300, 6, 7]
        run (construct plCatch) [100, 200, 300, 400] `shouldBe` [100, 2, 3, 300, 400, 6, 7]
