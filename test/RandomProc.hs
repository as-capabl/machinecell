{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

module
    RandomProc
where

import Prelude
import Control.Arrow.Machine as P
import Control.Arrow
import qualified Control.Category as Cat
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Test.QuickCheck (Arbitrary, arbitrary, oneof, frequency, sized)
import Data.Maybe (fromJust)
import System.Timeout (timeout)
import Control.DeepSeq (deepseq)
import System.IO.Unsafe (unsafePerformIO)


data ProcJoin = PjFst ProcGen | PjSnd ProcGen | PjSum ProcGen
              deriving Show

data ProcGen = PgNop | 
               PgStop |
               PgPush ProcGen |
               PgPop (ProcGen, ProcGen) ProcJoin |
               PgOdd ProcGen |
               PgDouble ProcGen |
               PgIncl ProcGen |
               PgHarf ProcGen
             deriving Show

instance
    Arbitrary ProcJoin
  where
    arbitrary = oneof [liftM PjFst arbitrary,
                      liftM PjSnd arbitrary,
                      liftM PjSum arbitrary]

instance 
    Arbitrary ProcGen
  where
    arbitrary = sized $ \i ->
                frequency [(40, rest), (40 + i, content)]
      where
        rest = return PgNop
        content = oneof [
                   return PgNop, 
                   return PgStop, 
                   liftM PgPush arbitrary,
                   liftM2 PgPop arbitrary arbitrary,
                   liftM PgOdd arbitrary,
                   liftM PgDouble arbitrary,
                   liftM PgIncl arbitrary,
                   liftM PgHarf arbitrary
                  ]
type MyProcT = ProcessA (Kleisli (State [Int]))

mkProc :: ProcGen 
       -> MyProcT (Event Int) (Event Int)


mkProc PgNop = Cat.id

mkProc (PgPush next) = mc >>> mkProc next
  where
    mc = repeatedly $
       do
         x <- await
         lift $ modify (\xs -> x:xs)
         yield x

mkProc (PgPop (fx, fy) fz) =
    mc >>> P.split >>> (mkProc fx *** mkProc fy) >>> mkProcJ fz
  where
    mc = repeatedly $
       do
         x <- await
         ys <- lift $ get
         case ys 
           of
             [] -> 
                 yield (Event x, NoEvent)
             (y:yss) -> 
               do 
                 lift $ put yss
                 yield (Event x, Event y)

mkProc (PgOdd next) = P.filter (arr cond) >>> mkProc next
  where
    cond x = x `mod` 2 == 1

mkProc (PgDouble next) = arr (fmap $ \x -> [x, x]) >>> fork >>> mkProc next

mkProc (PgIncl next) = arr (fmap (+1)) >>> mkProc next

mkProc (PgHarf next) = arr (fmap (`div`2)) >>> mkProc next

mkProc (PgStop) = stopped

mkProcJ :: ProcJoin -> MyProcT (Event Int, Event Int) (Event Int)

mkProcJ (PjFst pg) = arr fst
mkProcJ (PjSnd pg) = arr snd
mkProcJ (PjSum pg) = arr go
  where
    go (evx, evy) = (+) <$> evx <*> evy


stateProc :: MyProcT (Event a) (Event b) -> [a] -> ([b], [Int])
stateProc a i = 
    runState mx []
{-
    unsafePerformIO $ 
      do
        x <- timeout 10000 $
          do
            let x = runState mx []
            deepseq x $ return x
        return (fromJust x)
-}
  where
    mx = runKleisli (runProcessA a) i

class 
    TestIn a
  where
    input :: MyProcT (Event Int) a

class
    TestOut a
  where
    output :: MyProcT a (Event Int)

instance
    TestIn (Event Int)
  where
    input = Cat.id

instance
    TestOut (Event Int)
  where
    output = Cat.id

instance
    (TestIn a, TestIn b) => TestIn (a, b)
  where
    input = mc >>> P.split >>> input *** input
      where
        mc = repeatedly $
          do
            x <- await
            y <- await
            yield (Event x, Event y)

instance
    (TestOut a, TestOut b) => TestOut (a, b)
  where
    output = output *** output >>> P.join >>> mc >>> P.split
      where
        mc = repeatedly $
          do
            (x, y) <- await
            yield x
            yield y

instance
    (TestIn a, TestIn b) => 
        TestIn (Either a b)
  where
    input = proc evx ->
      do
        -- 一個前の値で分岐してみる
        b <- hold True <<< delay -< 
               (\x -> x `mod` 2 == 0) <$> evx

        if b
          then
            arr Left <<< input -< evx
          else
            arr Right <<< input -< evx


instance
    (TestOut a, TestOut b) => TestOut (Either a b)
  where
    output = output ||| output

type MyTestT a b = MyProcT a b -> MyProcT a b -> Bool

mkEquivTest :: (TestIn a, TestOut b) =>
               (Maybe (ProcGen, ProcJoin), ProcGen, ProcGen, [Int]) ->
               MyTestT a b
mkEquivTest (Nothing, pre, post, l) pa pb =
    let
        preA = mkProc pre
        postA = mkProc post
        mkCompared p = preA >>> input >>> p >>> output >>> postA
        x = stateProc (mkCompared pa) l
        y = stateProc (mkCompared pb) l
      in
        x == y

mkEquivTest (Just (par, j), pre, post, l) pa pb =
    let
        preA = mkProc pre
        postA = mkProc post
        parA = mkProc par
        joinA = mkProcJ j
        mkCompared p = preA >>> input >>> p >>> output >>> postA
        x = stateProc (mkCompared pa) l
        y = stateProc (mkCompared pb) l
      in
        x == y

mkEquivTest2 ::(Maybe (ProcGen, ProcJoin), ProcGen, ProcGen, [Int]) ->
               MyProcT (Event Int, Event Int) (Event Int, Event Int) -> 
               MyProcT (Event Int, Event Int) (Event Int, Event Int) ->
               Bool
mkEquivTest2 = mkEquivTest
