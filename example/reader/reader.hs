{-# LANGUAGE Arrows #-}

import Control.Arrow
import qualified Control.Arrow.Machine as P
import Control.Arrow.Transformer.Reader (ReaderArrow, elimReader)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans (lift)


mainPlan = P.constructT (P.reading P.kleisli . const) $
  do
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
	(\evy -> P.hold 0 -< evy)
  `P.feedback` \y ->
  do
	a <- P.hold 0 -< eva
	evy <- (| P.elimR (mainPlan -< eva) |) (a, y)
	returnA -< (evy, evy)

main = runKleisli (P.run mainProc) [1..]

