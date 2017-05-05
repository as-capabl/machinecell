{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module
    Types.ChoiceSpec
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

