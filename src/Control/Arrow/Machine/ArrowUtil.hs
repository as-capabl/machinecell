{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

-- | Arrow utilities not related to machinecell library.
module
    Control.Arrow.Machine.ArrowUtil (
        -- * Arrow construction helper
        ary0,
        ary1,
        ary2,
        ary3,
        ary4,
        ary5,

        kleisli,
        kleisli0,
        kleisli2,
        kleisli3,
        kleisli4,
        kleisli5,

        unArrowMonad,
        arrowMonad,

#if defined(MIN_VERSION_arrows)
        reading,
        statefully,
#endif

        -- * Arrow construction helper (Lens)
        -- |Lens Isomorphisms between arrows and monads.
        -- All definitions are defined arrow->monad directions.
        -- Use with lens operator (^.) and (#).
        kl,
        am,
#if defined(MIN_VERSION_arrows)
        rd,
#endif
        uc0,
        uc1,
        uc2,
        uc3,
        uc4,
        uc5,

        -- * Custom arrow syntax helper
        -- |To absorve arrow stack signature difference bettween ghc 7.8 and older.
        AS,
        toAS,
        fromAS,

#if defined(MIN_VERSION_arrows)
        elimR
#endif
    )
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
#if defined(MIN_VERSION_arrows)
import Control.Arrow.Operations (store, fetch)
import Control.Arrow.Transformer.Reader 
import Control.Arrow.Transformer.State
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.State (StateT, runStateT)
#endif
import Data.Profunctor

       
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

ary0 ::
    (forall p q. (p -> m q) -> a p q) ->
    m b ->
    a () b
ary0 f = f . const

ary1 ::
    (forall p q. (p -> m q) -> a p q) ->
    (a1 -> m b) ->
    a a1 b
ary1 f = f

ary2 ::
    (forall p q. (p -> m q) -> a p q) ->
    (a1 -> a2 -> m b) ->
    a (a1, a2) b
ary2 f fmx = f $ \(x1, x2) -> fmx x1 x2

ary3 ::
    (forall p q. (p -> m q) -> a p q) ->
    (a1 -> a2 -> a3 -> m b) ->
    a (a1, a2, a3) b
ary3 f fmx = f $ \(x1, x2, x3) -> fmx x1 x2 x3

ary4 ::
    (forall p q. (p -> m q) -> a p q) ->
    (a1 -> a2 -> a3 -> a4 -> m b) ->
    a (a1, a2, a3, a4) b
ary4 f fmx = f $ \(x1, x2, x3, x4) -> fmx x1 x2 x3 x4

ary5 ::
    (forall p q. (p -> m q) -> a p q) ->
    (a1 -> a2 -> a3 -> a4 -> a5 -> m b) ->
    a (a1, a2, a3, a4, a5) b
ary5 f fmx = f $ \(x1, x2, x3, x4, x5) -> fmx x1 x2 x3 x4 x5


kleisli :: Monad m => (a->m b) -> Kleisli m a b
kleisli = ary1 Kleisli

kleisli0 :: Monad m => m b -> Kleisli m () b
kleisli0 = ary0 Kleisli

kleisli2 :: Monad m => (a1 -> a2 -> m b) -> Kleisli m (a1, a2) b
kleisli2 = ary2 Kleisli

kleisli3 :: Monad m => (a1 -> a2 -> a3 -> m b) -> Kleisli m (a1, a2, a3) b
kleisli3 = ary3 Kleisli

kleisli4 :: Monad m => (a1 -> a2 -> a3 -> a4 -> m b) -> Kleisli m (a1, a2, a3, a4) b
kleisli4 = ary4 Kleisli

kleisli5 :: Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> m b) -> Kleisli m (a1, a2, a3, a4, a5) b
kleisli5 = ary5 Kleisli


unArrowMonad ::
    ArrowApply a =>
    (p -> ArrowMonad a q) -> a p q
unArrowMonad fmx = proc x -> case fmx x of { ArrowMonad a -> a } -<< ()

arrowMonad ::
    ArrowApply a =>
    a p q -> p -> ArrowMonad a q
arrowMonad af x = ArrowMonad $ arr (const x) >>> af


#if defined(MIN_VERSION_arrows)
reading :: 
    (Monad m, Arrow a) => 
    (forall p q. (p->m q)->a p q) -> 
    (b -> ReaderT r m c) ->
    ReaderArrow r a b c
reading f mr = ReaderArrow . f $ uncurry (runReaderT . mr)

statefully ::
    (Monad m, Arrow a) =>
    (forall p q. (p->m q)->a p q) -> 
    (b -> StateT s m c) ->
    StateArrow s a b c
statefully f ms = proc x ->
  do
    s <- fetch -< ()
    (y, s') <- liftState (f $ \(x, s) -> runStateT (ms x) s) -< (x, s)
    store -< s'
    returnA -< y
#endif

type MyIso s t a b =
    forall p f. (Profunctor p, Functor f) =>
    p a (f b) -> p s (f t)

type MyIso' s a = MyIso s s a a

myIso ::
    (s -> a) -> (b -> t) -> MyIso s t a b
myIso sa bt = dimap sa (fmap bt)

-- |Isomorphsm between m and (Kleisli m)
kl ::
    MyIso' (a -> m b) (Kleisli m a b)
kl = myIso Kleisli runKleisli

-- |Isomorphism between (ArrowMonad a) and a
am ::
    ArrowApply a =>
    MyIso' (b -> ArrowMonad a c) (a b c)
am = myIso unArrowMonad arrowMonad

#if defined(MIN_VERSION_arrows)
rd ::
    (Arrow a) =>
    (forall p q. MyIso' (p -> m q) (a p q)) ->
    MyIso' (b -> ReaderT r m c) (ReaderArrow r a b c)
rd f = e . f . g
  where
    e = myIso
        (\frmy -> uncurry (runReaderT . frmy))
        (\fmy -> ReaderT . (curry fmy))
    g = myIso ReaderArrow runReader
#endif

uc0 :: MyIso' (m b) (() -> m b)
uc0 = myIso const ($())

uc1 :: MyIso' (a1 -> m b) (a1 -> m b)
uc1 = id

uc2 :: MyIso' (a1 -> a2 -> m b) ((a1, a2) -> m b)
uc2 = myIso
    (\f (a1, a2) -> f a1 a2)
    (\f a1 a2 -> f (a1, a2))

uc3 :: MyIso' (a1 -> a2 -> a3 -> m b) ((a1, a2, a3) -> m b)
uc3 = myIso
    (\f (a1, a2, a3) -> f a1 a2 a3)
    (\f a1 a2 a3 -> f (a1, a2, a3))

uc4 :: MyIso' (a1 -> a2 -> a3 -> a4 -> m b) ((a1, a2, a3, a4) -> m b)
uc4 = myIso
    (\f (a1, a2, a3, a4) -> f a1 a2 a3 a4)
    (\f a1 a2 a3 a4 -> f (a1, a2, a3, a4))

uc5 :: MyIso' (a1 -> a2 -> a3 -> a4 -> a5 -> m b) ((a1, a2, a3, a4, a5) -> m b)
uc5 = myIso
    (\f (a1, a2, a3, a4, a5) -> f a1 a2 a3 a4 a5)
    (\f a1 a2 a3 a4 a5 -> f (a1, a2, a3, a4, a5))

#if defined(MIN_VERSION_arrows)
-- |Alternate for `elimReader` that can be used with both ghc 7.8 and older.
elimR ::
    ArrowAddReader r a a' =>
    a (AS e) b -> a' (e, AS r) b
elimR f =
    second (arr $ fromAS) >>> elimReader (arr toAS >>> f)
#endif

