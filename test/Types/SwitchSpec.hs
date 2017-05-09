{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module
    Types.SwitchSpec
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
    describe "kSwitch" $
      do
        it "switches spontaneously" $
          do
            let
                theArrow sw = sw (oneshot False) (arr snd) $ \_ _ -> oneshot True
            run (theArrow kSwitch) [] `shouldBe` [True]
            run (theArrow dkSwitch) [] `shouldBe` [False, True]
