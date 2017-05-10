{-# LANGUAGE Arrows #-}

module
    Types.LoopSpec
where

import Data.Functor
import Control.Arrow
import Test.Hspec

import Control.Arrow.Machine as P
import Control.Monad.Trans (liftIO)

import Data.IORef

import Common.RandomProc

doubler = arr (fmap $ \x -> [x, x]) >>> P.fork

spec =
  do
    it "is possible that value by `dHold` or `dAccum` can refer at upstream." $
      do
        ref <- newIORef ([] :: [Int])
        let
            pa :: ProcessT IO (Event Int) (Event ())
            pa = proc evx ->
              do
                rec
                    P.fire print -< y <$ evx
                    P.fire putStr -< "" <$ evx -- side effect
                    evx2 <- doubler -< evx
                    y <- P.dAccum 0 -< (+) <$> evx2
                fire (\x -> modifyIORef ref (x:)) -<  y <$ evx

        liftIO $ P.runT_ pa [1, 2, 3]
        ret <- readIORef ref
        reverse ret `shouldBe` [0, 1+1, 1+1+2+2]

    it "can be used with rec statement(pure)" $
      let
          a = proc ev ->
            do
              x <- hold 0 -< ev
              rec l <- returnA -< x:l
              returnA -< l <$ ev
          result = fst $ stateProc a [2, 5]
        in
          take 3 (result!!1) `shouldBe` [5, 5, 5]

    it "the last value is valid." $
      do
        let
            mc = repeatedly $
              do
                x <- await
                yield x
                yield (x*2)
            pa = proc x ->
              do
                rec y <- mc -< (+z) <$> x
                    z <- dHold 0 -< y
                returnA -< y
        run pa [1, 10] `shouldBe` [1, 2, 12, 24]

    it "carries no events to upstream." $
      do
        let
            pa = proc ev ->
              do
                rec r <- dHold True -< False <$ ev2
                    ev2 <- fork -< [(), ()] <$ ev
                returnA -< r <$ ev
        run pa [1, 2, 3] `shouldBe` [True, True, True]

