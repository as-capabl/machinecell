module
    RandomProc
where

import Control.Arrow.Machine
import Control.Arrow
import qualified Data.Machine as Mc
import qualified Control.Category as Cat
import Control.Monad.State
import Control.Monad
import Control.Monad.Trans
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
                      liftM PjSnd arbitrary]

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
    mc >>> arr sp >>> (mkProc fx *** mkProc fy) >>> mkProcJ fz
  where
    mc = repeatedly $
       do
         x <- await
         ys <- lift $ get
         lift $ modify pop
         yield (x, head_ ys)
    pop (x:xs) = xs
    pop [] = []
    head_ (x:xs) = x
    head_ [] = 0
    sp (Event (x,y)) = (Event x, Event y)
    sp NoEvent = (NoEvent, NoEvent)
    sp End = (End, End)

mkProc (PgOdd next) = mc >>> mkProc next
  where
    mc = repeatedly $
      do
        y <- await
        if y `mod` 2 == 1 then yield y else return ()

mkProc (PgDouble next) = mc >>> mkProc next
  where
    mc = repeatedly $
      do
        y <- await
        yield y
        yield y

mkProc (PgIncl next) = arr (fmap (+1)) >>> mkProc next

mkProc (PgStop) = toProcessA Mc.Stop

mkProcJ :: ProcJoin -> MyProcT (Event Int, Event Int) (Event Int)

mkProcJ (PjFst pg) = arr fst
mkProcJ (PjSnd pg) = arr snd


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
