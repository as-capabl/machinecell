
module
    Control.Arrow.Machine.ArrowUtil (
        kleisli,
        kleisli0,
        kleisli2,
        kleisli3,
        kleisli4,
        kleisli5
    )
where

import Control.Arrow


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


