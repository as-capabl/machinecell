{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module
    Types.StepExecutionSpec
where

import qualified Control.Arrow.Machine as P

import Data.Maybe (isJust)
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

-- | Represents return values and informations of step executions.
data ExecInfo a =
    ExecInfo
      {
        yields :: [a], -- ^ Values yielded while the step.
        hasConsumed :: Bool, -- ^ True if the input value is consumed.
            --
            -- False if the machine has stopped unless consuming the input.
            --
            -- Or in the case of `stepYield`, this field become false when
            -- the machine produces a value unless consuming the input.
        hasStopped :: Bool -- ^ True if the machine has stopped at the end of the step.
      }
    deriving (Eq, Show)


spec =
  do
    it "supports step execution" $
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

            emptyEI = ExecInfo
              {
                yields = [],
                hasConsumed = True, -- Set False if there's any leftover
                hasStopped = False
              }

            onYield x =
                modify $ \ei@ExecInfo{yields = xs} -> ei {yields = xs ++ [x]}

            onStop ma =
                modify $ \ei -> ei {hasConsumed = not (isJust ma), hasStopped = True}

        -- execution part
        --   x <- await
        --   yield x
        --   yield (x+1)
        (now, ret) <- runStateT (stepRun lift onYield onStop init 1) emptyEI
        yields ret `shouldBe` [1, 2]
        hasConsumed ret `shouldBe` True
        hasStopped ret `shouldBe` False

        -- execution part
        --   x <- await
        --   yield x
        --   yield (x+1)
        --   yield (x+5)
        (now, ret) <- runStateT (stepRun lift onYield onStop now 1) emptyEI
        yields ret `shouldBe` [1, 2, 6]
        hasConsumed ret `shouldBe` True
        hasStopped ret `shouldBe` True

        -- no execution part is left
        (now, ret) <- runStateT (stepRun lift onYield onStop now 1) emptyEI
        yields ret `shouldBe` ([]::[Int])
        hasConsumed ret `shouldBe` False
        hasStopped ret `shouldBe` True

        -- execution part
        --   _ <- await
        --   return ()
        (now, ret) <- runStateT (stepRun lift onYield onStop init2 1) emptyEI
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

            emptyEI = ExecInfo
              {
                yields = [], -- Not used
                hasConsumed = False,
                hasStopped = False
              }

            provide x =
              do
                modify $ \ei -> ei {hasConsumed = True}
                return x

            onStop =
                modify $ \ei -> ei {hasStopped = True}

        -- execution part
        --   yield (-1)
        ((val, now), ret) <- runStateT (stepYield lift (provide 5) onStop init) emptyEI
        val `shouldBe` Just (-1)
        hasConsumed ret `shouldBe` False
        hasStopped ret `shouldBe` False

        -- execution part
        --   _ <- await
        ((val, now), ret) <- runStateT (stepYield lift (provide 6) onStop now) emptyEI
        val `shouldBe` Nothing
        hasConsumed ret `shouldBe` True
        hasStopped ret `shouldBe` False

        -- execution part
        --   x <- await
        --   mapM yield (iterate (+1) x) -- first one
        ((val, now), ret) <- runStateT (stepYield lift (provide 10) onStop now) emptyEI
        val `shouldBe` Just 10
        hasConsumed ret `shouldBe` True
        hasStopped ret `shouldBe` False

        -- execution part
        --   mapM yield (iterate (+1) x) -- second one
        ((val, now), ret) <- runStateT (stepYield lift (provide 10) onStop now) emptyEI
        val `shouldBe` Just 11
        hasConsumed ret `shouldBe` False
        hasStopped ret `shouldBe` False

        -- execution part
        --   return ()
        ((val, now), ret) <- runStateT (stepYield lift (provide 0) onStop init2) emptyEI
        val `shouldBe` (Nothing :: Maybe Int)
        hasConsumed ret `shouldBe` False
        hasStopped ret `shouldBe` True

        -- execution part
        --   _ <- await
        --   return ()
        ((val, now), ret) <- runStateT (stepYield lift (provide 0) onStop init3) emptyEI
        val `shouldBe` (Nothing :: Maybe Int)
        hasConsumed ret `shouldBe` True
        hasStopped ret `shouldBe` True


