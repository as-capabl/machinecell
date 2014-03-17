module
    RandomProc
where

import Prelude hiding (filter)
import Control.Arrow.Machine
import Control.Arrow
import qualified Data.Machine as Mc
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
               PgIncl ProcGen
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
                   liftM PgIncl arbitrary
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
    mc >>> split >>> (mkProc fx *** mkProc fy) >>> mkProcJ fz
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

mkProc (PgOdd next) = filter (arr cond) >>> mkProc next
  where
    cond x = x `mod` 2 == 1

mkProc (PgDouble next) = arr (fmap $ take 2 . repeat) >>> fork >>> mkProc next

mkProc (PgIncl next) = arr (fmap (+1)) >>> mkProc next

mkProc (PgStop) = toProcessA Mc.Stop

mkProcJ :: ProcJoin -> MyProcT (Event Int, Event Int) (Event Int)

mkProcJ (PjFst pg) = arr fst
mkProcJ (PjSnd pg) = arr snd
mkProcJ (PjSum pg) = arr go
  where
    go (evx, evy) = (+) <$> evx <*> evy


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
