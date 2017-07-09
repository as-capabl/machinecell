{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module
    Utils.SourceSpec
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

spec =
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
                    s1 <- P.blockingSource [1, 2, 3] -< mempty
                    s2 <- P.blockingSource [4, 5, 6] -< mempty
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
                (mempty >>> P.blockingSource l)
                    `equiv` (mempty >>> P.blocking (P.source l))


