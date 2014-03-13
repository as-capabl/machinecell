{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module
    Control.Arrow.Machine.Types
where

import qualified Data.Machine as Mc
import qualified Control.Category as Cat
import Control.Monad (liftM)
import Control.Arrow
import Control.Applicative
import Control.Monad.Trans
import Debug.Trace
import Data.Maybe

import Control.Arrow.Machine.Event
import Control.Arrow.Machine.Detail


data
    ProcessA a b c
  where
    Pure :: (b->c) -> ProcessA a b c
    Machine :: Mc.Machine (a b) c -> ProcessA a (Event b, d) (Event c, d)
    Concat :: ProcessA a b d -> ProcessA a d c -> ProcessA a b c


toProcessA :: ArrowApply a => Mc.Machine (a b) c -> ProcessA a (Event b) (Event c)
toProcessA mc = arr (\x -> (x, ())) >>> Machine mc >>> arr fst

instance
    ArrowApply a => Cat.Category (ProcessA a)
  where
    id = Pure id
    g . f = Concat f g

instance 
    ArrowApply a => Arrow (ProcessA a)
  where
    arr = Pure

    first (Pure f) = Pure $ first f
    first (Machine mc) = arr assoc >>> Machine mc >>> arr unassoc
      where
        assoc ((evx, d1), d2) = (evx, (d1, d2))
        unassoc (evx, (d1, d2)) = ((evx, d1), d2)
    first (Concat f g) = first f >>> first g

feed :: ArrowApply a => Bool -> ProcessA a b c -> a b (c, ProcessA a b c, Bool)

feed ex pa@(Pure f) = arr (\x -> (f x, pa, ex))

feed True pa@(Machine (Mc.Await fc f ff)) = proc (evx, d) ->
  do
    (| hEv' 
        (\x -> 
          do
            y <- f -< x
            oneYield True d (fc y) -<< ())
        (returnA -< ((NoEvent, d), pa, False))
        (oneYield True d ff -<< ())
      |) evx

feed ex (Machine mc) = proc (_, d) ->
  do
    oneYield ex d mc -<< ()

feed ex (Concat pa pb) = proc x ->
  do
    (r1, pa', ex1) <- feed ex pa -< x
    (r2, pb', ex2) <- feed ex1 pb -<< r1
    returnA -< (r2, Concat pa' pb', ex2)

oneYield ex d (Mc.Yield y fc) = proc _ ->
    returnA -< ((Event y, d), Machine fc, True)

oneYield ex d Mc.Stop = proc _ ->
    returnA -< ((End, d), Machine Mc.Stop, ex)

oneYield ex d mc = proc _ ->
    returnA -< ((NoEvent, d), Machine mc, ex)


runProcessA :: ArrowApply a => ProcessA a (Event b) (Event c) -> a [b] [c]
runProcessA pa = proc xs -> 
  do
    ys <- go False pa xs id -<< ()
    returnA -< ys []
  where
    go True pa [] ys = arr $ const ys

    go False pa [] ys = proc _ ->
      do
        (y, pa', ex') <- feed False pa -< End
        react y ex' pa' [] ys -<< ()

    go ex pa (x:xs) ys = proc _ ->
      do
        let (evx, xs') = if ex then (Event x, xs) else (NoEvent, x:xs)
        (y, pa', ex') <- feed ex pa -< evx
        react y ex pa' xs' ys -<< ()
    
    react End ex pa xs ys =
      do
        go (not ex) pa [] ys

    react (Event y) ex pa xs ys =
        go (not ex) pa xs (\cont -> ys (y:cont))

    react NoEvent ex pa xs ys =
        go (not ex) pa xs ys

{-
    first (ProcessA f) = ProcessA $ \prdc -> unassocA $ f (assocA prdc)
      where
        assocA (ProcessA_ pre post mc) = ProcessA_ pre (assoc . post) mc
        unassocA (ProcessA_ pre post mc) = ProcessA_ pre (unassoc . post) mc

        assoc (Just (x, y), z) = (Just x, (Just y, z))
        assoc (Nothing, z) = (Nothing, (Nothing, z))

        unassoc (Just x, (Just y, z)) = (Just (x, y), z)
        unassoc (_, (_, z)) = (Nothing, z)
-}

{-
traceMc = const id
--traceMc = trace


data ProcessA a b c =
               ProcessA (forall t d. ProcessA_ a t (Maybe b, d) -> ProcessA_ a t (Maybe c, d))

toProcessA :: ArrowApply a => Mc.Machine (a b) c -> ProcessA a (Event b) (Event c)
toProcessA mc = ProcessA $ \prdc -> concatenate prdc (ProcessA_ pre post mc)
  where
    pre (Just x, y) = (x, Left y)
    pre (Nothing, y) = (NoEvent, Right y)
    post (x, Left y) = (Just x, y)
    post (_, Right y) = (Nothing, y)

{-
instance 
    Arrow a => Functor (a b)
  where 
    fmap f m = arr f <<< m

instance
    Arrow a => Mc.Await b (a b)
  where
    await = Cat.id
-}
awaitA :: Arrow a => Mc.Plan c (a b) b
awaitA = Mc.request $ Cat.id

{-
instance 
    MonadTrans (Mc.Plan c Machine (Kleisli [!!!] b))
  [!!!]が最後に来ないとMonadTransのインスタンスに出来ない
-}
-- liftM :: Monad m => m d -> Mc.Plan c (Kleisli m b) d
-- liftM mx = Mc.request $ Kleisli $ const mx
-- ↑は必ず入力を一つ握り潰してしまう




runProcessA :: ArrowApply a => ProcessA a (Event b) (Event c) -> a [b] [c]

runProcessA pa = 
    runProcessA_ $ resolveCPS pa

resolveCPS :: Arrow a => ProcessA a (Event b) (Event c) -> ProcessA_ a (Event b) (Event c)
resolveCPS (ProcessA cps) = dropMc $ cps (ProcessA_ pre1 post1 mc1)
  where
    pre1 evx = (evx, ())
    post1 (evx, _) = (Just evx, ())
    mc1 = Mc.pass Cat.id

    dropMc (ProcessA_ pre post mc) = ProcessA_ pre (dropImpl . post) mc
    dropImpl (Just evc, _) = evc
    dropImpl (Nothing, _) = NoEvent





instance
    ArrowApply a => Cat.Category (ProcessA a)
  where
    id = ProcessA id
    ProcessA f . ProcessA g = ProcessA (f . g)

instance 
    ArrowApply a => Arrow (ProcessA a)
  where
    arr f = ProcessA $ arrImpl (fmap f)
    first (ProcessA f) = ProcessA $ \prdc -> unassocA $ f (assocA prdc)
      where
        assocA (ProcessA_ pre post mc) = ProcessA_ pre (assoc . post) mc
        unassocA (ProcessA_ pre post mc) = ProcessA_ pre (unassoc . post) mc

        assoc (Just (x, y), z) = (Just x, (Just y, z))
        assoc (Nothing, z) = (Nothing, (Nothing, z))

        unassoc (Just x, (Just y, z)) = (Just (x, y), z)
        unassoc (_, (_, z)) = (Nothing, z)


arrImpl :: (b->c) -> ProcessA_ a t (b,d) -> ProcessA_ a t (c,d)
arrImpl f (ProcessA_ pre post mc) = ProcessA_ pre (first f . post) mc



instance
    ArrowApply a => ArrowChoice (ProcessA a)
  where
    left (ProcessA f) = ProcessA $ \prdc -> unresolveA (f (resolveA prdc))
      where
        resolveA (ProcessA_ pre post mc) = 
            ProcessA_ pre (resolve . post) mc
        unresolveA (ProcessA_ pre post mc) = 
            ProcessA_ pre (unresolve . post) mc

        resolve (Just (Left c), d) = (Just c, Left d)
        resolve (Nothing, d) = (Nothing, Left d)
        resolve (Just (Right x), d) = (Nothing, Right (x, d))

        unresolve (Just x, Left d) = (Just (Left x), d)
        unresolve (Nothing, Left d) = (Nothing, d)
        unresolve (_, Right (x, d)) = (Just (Right x), d)




instance
    (ArrowApply a, ArrowLoop a) => ArrowLoop (ProcessA a)
  where
    loop (ProcessA f) = 
        ProcessA $ 
          \prcd -> 
              loopProcessA_ (swap_ $ f $ first_ prcd)
      where
        first_ :: ProcessA_ a t (Maybe b, p)-> 
                 ProcessA_ a (t, d) (Maybe (b, d), Either p (p, d)) 
        first_ (ProcessA_ pre post mc) = 
            ProcessA_ (assoca pre) (unassoca post) mc
        assoca pre (x, d) = 
            let (ev, r) = pre x in (ev, (r, d))
        unassoca post (evx, (r, d)) = 
            let
                (mb, p) = post (evx, r)
              in
                case mb 
                  of 
                    Just x -> (Just (x, d), Left p)
                    Nothing -> (Nothing, Right (p, d))


        swap_ :: ProcessA_ a (t, d) (Maybe (b, d), Either p (p, d)) -> 
                 ProcessA_ a (t, d) ((Maybe b, p), d)
        swap_ (ProcessA_ pre post mc) = ProcessA_ pre (sw . post) mc
        sw (Just (b, d), Left p) = ((Just b, p), d)
        sw (_, Right (p, d)) = ((Nothing, p), d)


-}
