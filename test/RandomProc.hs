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

mkProc (PgPush next) = toProcessA mc >>> mkProc next
  where
    mc = Mc.repeatedly $
       do
         y <- Mc.request $ 
              Kleisli (\x -> do{ modify (\xs -> x:xs); return x})
         Mc.yield y

mkProc (PgPop (fx, fy) fz) =
    toProcessA mc >>> arr sp >>> (mkProc fx *** mkProc fy) >>> mkProcJ fz
  where
    mc = Mc.repeatedly $
       do
         z <- Mc.request $ 
              Kleisli (\x -> 
                do
                  ys <- get
                  modify pop
                  return (x, head_ ys))
         Mc.yield z
    pop (x:xs) = xs
    pop [] = []
    head_ (x:xs) = x
    head_ [] = 0
    sp (Event (x,y)) = (Event x, Event y)
    sp NoEvent = (NoEvent, NoEvent)

mkProc (PgOdd next) = toProcessA mc >>> mkProc next
  where
    mc = Mc.repeatedly $
      do
        y <- awaitA
        if y `mod` 2 == 1 then Mc.yield y else return ()

mkProc (PgDouble next) = toProcessA mc >>> mkProc next
  where
    mc = Mc.repeatedly $
      do
        y <- awaitA
        Mc.yield y
        Mc.yield y

mkProc (PgIncl next) = arr (fmap (+1)) >>> mkProc next

mkProc (PgStop) = toProcessA Mc.Stop

mkProcJ :: ProcJoin -> MyProcT (Event Int, Event Int) (Event Int)

mkProcJ (PjFst pg) = arr fst
mkProcJ (PjSnd pg) = arr snd


stateProc a i = 
    runState mx []
  where
    mx = runKleisli (runProcessA a) i
