{-# LANGUAGE Arrows #-}

import Control.Arrow
import qualified Control.Arrow.Machine as P
import Control.Arrow.Transformer.Reader (ReaderArrow, elimReader)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans (lift)
import Control.Lens


mainPlan = P.constructT (^. P.uc0 . P.rd P.kl) $
  do
    P.await
    (a, y) <- ask
    lift $ lift $ putStrLn $ "a" ++ show (a::Int, y::Int)
    P.await
    (a, y) <- ask
    lift $ lift $ putStrLn $ "b" ++ show (a, y)
    P.await
    (a, y) <- ask
    lift $ lift $ putStrLn $ "c" ++ show (a, y)
    P.yield 1
    (a, y) <- ask
    lift $ lift $ putStrLn $ "d" ++ show (a, y)
    P.await
    (a, y) <- ask
    lift $ lift $ putStrLn $ "e" ++ show (a, y)
    P.yield 2
    (a, y) <- ask
    lift $ lift $ putStrLn $ "f" ++ show (a, y)
    P.yield 3
    (a, y) <- ask
    lift $ lift $ putStrLn $ "g" ++ show (a, y)

mainProc = proc eva ->
  do
    rec
        a <- P.hold 0 -< eva
        evy <- P.readerProc mainPlan -< (eva, (a, y))
        y <- P.dHold 0 -< evy
    returnA -< evy

main = return [1..] >>= P.kl # P.run mainProc

