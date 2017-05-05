{-# LANGUAGE Arrows #-}

module
    LoopUtil
where

import Data.Functor
import Control.Arrow
import Test.Hspec

import Control.Category ((>>>))

import Control.Arrow.Machine as P
import Control.Monad.Trans (liftIO)
import qualified Control.Arrow.Machine.Misc.Pump as Pump

import Data.Monoid (Endo(Endo), mappend, appEndo)

newtype Duct a = Duct (Endo [a])

doubler = arr (fmap $ \x -> [x, x]) >>> P.fork

loopUtil =
  do
    describe "loop" $
      do
        it "is possible that value by `dHold` or `dAccum` can refer at upstream." $
          do
            let
                pa :: ProcessT IO (Event Int) (Event Int)
                pa = proc evx ->
                  do
                    rec
                        P.fire print -< y <$ evx
                        P.fire putStr -< "" <$ evx -- side effect
                        evx2 <- doubler -< evx
                        y <- P.dAccum 0 -< (+) <$> evx2
                    returnA -< y <$ evx
            ret <- liftIO $ P.runT pa [1, 2, 3]
            ret `shouldBe` [0, 1+1, 1+1+2+2]

    describe "Pump" $
      do
        it "pumps up an event stream." $
          do
            let
                pa :: ProcessT IO (Event Int) (Event Int)
                pa = proc evx ->
                  do
                    rec
                        evOut <- Pump.outlet -< (dct, () <$ evx)
                        fire putStr -< "" <$ evx -- side effect
                        so <- doubler -< evx
                        dct <- Pump.intake -< (so, () <$ evx)
                    returnA -< evOut

            ret <- liftIO $ P.runT pa [4, 5, 6]
            ret `shouldBe` [4, 4, 5, 5, 6, 6]


