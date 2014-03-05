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


{-
instance
    ArrowApply a => ArrowChoice (ProcessA a)
  where
    left (ProcessA f) = eith >>> ProcessA $ \succ -> f $ unassocA succ
      where
        eith = undefined

        assocA (ProcessA_ pre post mc) = ProcessA_ (assoc pre) (assoc2 post) mc
        unassocA (ProcessA_ pre post mc) = ProcessA_ (pre . unassoc) post mc


        assoc pre (Just (Left x), z) = 
            case pre (Just x, z) of (evp, c) -> (evp, Left (c, z))
        assoc pre (Just (Right d), z) = 
            case pre (Nothing, z) of (evp, c) -> (evp, Right (c, d, z))


--        assoc2 post (evq, Left (c, z)) = (Just $ post (evq, c), Left z)
--        assoc2 post (evq, Right (c, d, z)) = (Nothing, Right (d, z))
        assoc2 = id

        unassoc (Just x', Left z) = (Just (Left x') , z)
        unassoc (_, Right (d, z)) = (Just (Right d), z)
-}



instance
    (ArrowApply a, ArrowLoop a) => ArrowLoop (ProcessA a)
  where
    loop (ProcessA f) = 
        ProcessA $ 
          \prcd -> 
              loopImpl (swap_ $ f $ first_ prcd)
      where
        -- assocA (ProcessA_ pre post mc) = ProcessA_ (pre . assoc) post mc
        -- unassocA (ProcessA_ pre post mc) = ProcessA_ (pre . unassoc) post mc
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
        -- unassoc (x, (y, z)) = ((x, y), z)

loopImpl ::
    (ArrowApply a, ArrowLoop a) =>
    ProcessA_ a (b, d) (c, d) -> ProcessA_ a b c

loopImpl (ProcessA_ pre post Mc.Stop) =
    ProcessA_ pre' post' Mc.Stop
  where
    pureLoop (evq, x) = 
        let 
            (evp, r) = pre (x, d)
            (y, d) = post (evq, r)
          in
            (evp, y)
            
    pre' x = pureLoop (NoEvent, x)

    post' (_, pure) = pure

loopImpl (ProcessA_ pre post mc) =
    ProcessA_ pre' post' (loopMc pureLoop mc)
  where
    pureLoop (evq, x) = 
        let 
            (evp, r) = pre (x, d)
            (y, d) = post (evq, r)
          in
            (evp, y)
            
    pre' x = (Event x, x)

    post' (evq, x) = 
        case evq of 
          Event y -> y
          NoEvent -> snd $ pureLoop (NoEvent, x)
          End -> snd $ pureLoop (End, x)

loopMc :: 
    (ArrowLoop a, ArrowApply a) =>
    ((Event q, b)->(Event p, c)) ->
    Mc.Machine (a p) q ->
    Mc.Machine (a b) c
loopMc pureLoop mc@(Mc.Await _ _ ff) = 
    Mc.Await 
      (id)
      (theArrow pureLoop mc) 
      (loopMc pureLoop ff)

loopMc pureLoop Mc.Stop = Mc.Stop

theArrow :: 
    (ArrowLoop a, ArrowApply a) =>
    ((Event q, b)->(Event p, c)) ->
    Mc.Machine (a p) q ->
    a b (Mc.Machine (a b) c)

theArrow pureLoop mc@(Mc.Await fc f _) =
      proc b ->
        do
          (c, mc'') <- loop core -< b
          let
            (yields, mcRet) = restYield pureLoop b mc'' 
          returnA -< (Mc.Yield c . yields) (loopMc pureLoop mcRet)
  where
    core = 
      proc (b, evq) ->
        do
          let (evp, c) = pureLoop (evq, b)
          mc' <- (| hEv' 
                    (\p -> arr fc <<< f -< p)
                    (returnA -< mc)
                    (returnA -< Mc.Stop)
                  |) evp
          let (evq', mc'') = oneYield mc'
          returnA -< ((c, mc''), evq')

theArrow pureLoop Mc.Stop =
      proc b ->
        do
          (evp, c) <- arr pureLoop -< (NoEvent, b)
          returnA -< next c evp
  where
    next c NoEvent = Mc.Yield c $ loopMc pureLoop Mc.Stop
    next _ _ = Mc.Stop

-- theArrow :: (ArrowApply a, ArrowLoop a) => a (x, i) (mc, o)
normal (Mc.Await fc f ff) = f >>> arr (\x -> oneYield $ fc x)
-- oneYield (Mc.Yield (x, d) ff) = ((x, ff), d)
oneYield (Mc.Yield x mc') = (Event x, mc')
oneYield mc = (NoEvent, mc)

restYield pureLoop b (Mc.Yield x mc') = 
    let
        (_, c) = pureLoop (Event x, b)
        (yields, mcRet) = restYield pureLoop b mc'
      in
        (Mc.Yield c . yields, mcRet)

restYield _ _ mc = (id, mc)

