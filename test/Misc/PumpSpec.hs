{-# LANGUAGE Arrows #-}

module
    Misc.PumpSpec
where

import Data.Functor
import Control.Arrow
import Test.Hspec

import Control.Category ((>>>))

import Control.Arrow.Machine as P
import Control.Arrow.Machine.AdditionalInstances
import Control.Monad.Trans (liftIO)
import qualified Control.Arrow.Machine.Misc.Pump as Pump

import Data.Monoid (Endo(Endo), mappend, appEndo)

import Data.IORef

newtype Duct a = Duct (Endo [a])

doubler = arr (fmap $ \x -> [x, x]) >>> P.fork

spec =
  do
    it "pumps up an event stream." $
      do
        ref <- newIORef ([] :: [Int])
        let
            pa :: ProcessT IO (Event Int) (Event ())
            pa = proc evx ->
              do
                rec
                    evOut <- Pump.outlet -< (dct, () <$ evx)
                    fire (putStr) -< "" <$ evx -- side effect
                    so <- doubler -< evx
                    dct <- Pump.intake -< (so, () <$ evx)
                fire (\x -> modifyIORef ref (x:)) -< evOut

        liftIO $ P.runT_ pa [4, 5, 6]
        ret <- readIORef ref
        reverse ret `shouldBe` [4, 4, 5, 5, 6, 6]


