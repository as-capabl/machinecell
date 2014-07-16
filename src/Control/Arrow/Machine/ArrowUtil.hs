{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}


module
    Control.Arrow.Machine.ArrowUtil (
        kleisli,
        kleisli0,
        kleisli2,
        kleisli3,
        kleisli4,
        kleisli5,

        AS,
        toAS,
        fromAS,

        reading,

        elimR
    )
where

import Control.Arrow
import Control.Arrow.Operations (readState)
import Control.Arrow.Transformer.Reader
import Control.Monad.Reader (ReaderT, runReaderT)

-- To absorve 
#if __GLASGOW_HASKELL__ >= 708

type AS e = (e, ())

toAS :: e -> AS e
toAS e = (e, ())

fromAS :: AS e -> e
fromAS = fst

#else

type AS e = e

toAS :: e -> AS e
toAS = id

fromAS :: AS e -> e
fromAS = id

#endif


kleisli :: Monad m => (a->m b) -> Kleisli m a b
kleisli = Kleisli

kleisli0 :: Monad m => m b -> Kleisli m () b
kleisli0 = Kleisli . const

kleisli2 :: Monad m => (a1 -> a2 -> m b) -> Kleisli m (a1, a2) b
kleisli2 fmx = Kleisli $ \(x1, x2) -> fmx x1 x2

kleisli3 :: Monad m => (a1 -> a2 -> a3 -> m b) -> Kleisli m (a1, a2, a3) b
kleisli3 fmx = Kleisli $ \(x1, x2, x3) -> fmx x1 x2 x3

kleisli4 :: Monad m => (a1 -> a2 -> a3 -> a4 -> m b) -> Kleisli m (a1, a2, a3, a4) b
kleisli4 fmx = Kleisli $ \(x1, x2, x3, x4) -> fmx x1 x2 x3 x4

kleisli5 :: Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> m b) -> Kleisli m (a1, a2, a3, a4, a5) b
kleisli5 fmx = Kleisli $ \(x1, x2, x3, x4, x5) -> fmx x1 x2 x3 x4 x5


reading :: 
    (Monad m, Arrow a) => 
    (forall p q. (p->m q)->a p q) -> 
    (b -> ReaderT r m c) -> ReaderArrow r a b c
reading f mr = proc x ->
  do
    r <- readState -< ()
    liftReader (f $ \(x, r) -> runReaderT (mr x) r) -< (x, r)


elimR ::
    ArrowAddReader r a a' =>
    a (AS e) b -> a' (e, AS r) b
elimR f =
    second (arr $ fromAS) >>> elimReader (arr toAS >>> f)
