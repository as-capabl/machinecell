{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module
    Types.StepExecutionSpec
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

        pl2 =
          do
            _ <- await
            return ()
        init2 = construct pl2

    it "supports step execution" $
      do
        -- execution part
        --   x <- await
        --   yield x
        --   yield (x+1)
        (ret, now) <- stepRun init 1
        yields ret `shouldBe` [1, 2]
        hasConsumed ret `shouldBe` True
        hasStopped ret `shouldBe` False

        -- execution part
        --   x <- await
        --   yield x
        --   yield (x+1)
        --   yield (x+5)
        (ret, now) <- stepRun now 1
        yields ret `shouldBe` [1, 2, 6]
        hasConsumed ret `shouldBe` True
        hasStopped ret `shouldBe` True

        -- no execution part is left
        (ret, _) <- stepRun now 1
        yields ret `shouldBe` ([]::[Int])
        hasConsumed ret `shouldBe` False
        hasStopped ret `shouldBe` True

        -- execution part
        --   _ <- await
        --   return ()
        (ret, now) <- stepRun init2 1
        yields ret `shouldBe` ([]::[Int])
        hasConsumed ret `shouldBe` True
        hasStopped ret `shouldBe` True

    it "supports yield-driven step" $
      do
        let
            init = construct $
              do
                yield (-1)
                _ <- await
                x <- await
                mapM yield (iterate (+1) x) -- infinite
            init2 = construct $
              do
                return ()
            init3 = construct $
              do
                _ <- await
                return ()

        -- execution part
        --   yield (-1)
        (ret, now) <- stepYield init 5
        yields ret `shouldBe` Just (-1)
        hasConsumed ret `shouldBe` False
        hasStopped ret `shouldBe` False

        -- execution part
        --   _ <- await
        (ret, now) <- stepYield now 6
        yields ret `shouldBe` Nothing
        hasConsumed ret `shouldBe` True
        hasStopped ret `shouldBe` False

        -- execution part
        --   x <- await
        --   mapM yield (iterate (+1) x) -- first one
        (ret, now) <- stepYield now 10
        yields ret `shouldBe` Just 10
        hasConsumed ret `shouldBe` True
        hasStopped ret `shouldBe` False

        -- execution part
        --   mapM yield (iterate (+1) x) -- second one
        (ret, now) <- stepYield now 10
        yields ret `shouldBe` Just 11
        hasConsumed ret `shouldBe` False
        hasStopped ret `shouldBe` False

        -- execution part
        --   return ()
        (ret, _) <- stepYield init2 0
        yields ret `shouldBe` (Nothing :: Maybe Int)
        hasConsumed ret `shouldBe` False
        hasStopped ret `shouldBe` True

        -- execution part
        --   _ <- await
        --   return ()
        (ret, _) <- stepYield init3 0
        yields ret `shouldBe` (Nothing :: Maybe Int)
        hasConsumed ret `shouldBe` True
        hasStopped ret `shouldBe` True


