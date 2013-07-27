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

import Control.Arrow.Machine.Event
import Control.Arrow.Machine.Detail

traceMc = const id
--traceMc = trace


data ProcessA a b c =
               ProcessA (forall t d. ProcessA_ a (c, d) t -> ProcessA_ a (b, d) t)

toProcessA :: ArrowApply a => Mc.Machine (a b) c -> ProcessA a (Event b) (Event c)
toProcessA mc = ProcessA $ \succ -> concatenate (ProcessA_ pre post mc) succ
  where
    pre = id
    post = id

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
awaitA :: Arrow a => Mc.Plan b (a b) b
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
resolveCPS (ProcessA cps) = dropIt $ cps $ ProcessA_ pre1 post1 mc1
  where
    pre1 = id
    post1 = fst
    mc1 = Mc.pass Cat.id

    dropIt (ProcessA_ pre post mc) = ProcessA_ (pre . pad) post mc
    pad x = (x, error "This may not be touched.")


{-
-- bind
bind Mc.Stop = arr $ const Mc.Stop

bind (Mc.Yield y gc) = proc x ->
  do
    gc' <- bind gc -< x
    returnA -< Mc.Yield y gc' 

bind (Mc.Await gc g gf) = proc x ->
  do
    y <- g -< x
    returnA -< gc y


--successively
successive (Mc.Yield x fc) g = Mc.Yield x $ successive fc g

successive (Mc.Await fc m ff) g = Mc.Await (\x -> successive (fc x) g) m (successive ff g)

successive Mc.Stop g = g
-}


instance
    ArrowApply a => Cat.Category (ProcessA a)
  where
    id = ProcessA id
    ProcessA f . ProcessA g = ProcessA (g . f)

instance 
    ArrowApply a => Arrow (ProcessA a)
  where
    arr f = ProcessA $ arrImpl f
    first (ProcessA f) = ProcessA $ \succ -> assocA $ f $ unassocA succ
      where
        assocA (ProcessA_ pre post mc) = ProcessA_ (pre . assoc) post mc
        unassocA (ProcessA_ pre post mc) = ProcessA_ (pre . unassoc) post mc
        assoc ((x, y), z) = (x, (y, z))
        unassoc (x, (y, z)) = ((x, y), z)


arrImpl :: (b->c) -> ProcessA_ a (c,d) t0 -> ProcessA_ a (b,d) t0
arrImpl f (ProcessA_ pre post mc) = ProcessA_ (pre . first f) post mc


{-
instance
    ArrowApply a => ArrowChoice (ProcessA a)
  where
    left (ProcessA f) = ProcessA $ \succ -> assocA $ f $ unassocA succ
      where
        assocA (ProcessA_ pre post mc) = ProcessA_ (assoc pre) post mc
        unassocA (ProcessA_ pre post mc) = ProcessA_ (pre . unassoc) post mc
        assoc pre (Left x, z) = 
            case pre x of (evx, c) -> (evx, (Left c, z))
        assoc _ (Right y, z) = (NoEvent, (Right y, z))
        unassoc (_, (Event y, z)) = (Right y, z)
        unassoc (x', (NoEvent, z)) = (Left x' , z)
-}

instance
    (ArrowApply a, ArrowLoop a) => ArrowLoop (ProcessA a)
  where
    loop (ProcessA f) = 
        ProcessA $ 
          \succ -> 
              loopImpl (swap_ $ f $ first_ succ)
      where
        -- assocA (ProcessA_ pre post mc) = ProcessA_ (pre . assoc) post mc
        -- unassocA (ProcessA_ pre post mc) = ProcessA_ (pre . unassoc) post mc
        first_ :: ProcessA_ a (c, p) t-> 
                 ProcessA_ a ((c, d), p) (t, d)
        first_ (ProcessA_ pre post mc) = 
            ProcessA_ (assoca pre) (unassoca post) mc
        assoca pre ((x, d), p) = let (ev, r) = pre (x, p) in (ev, (r, d))
        unassoca post (evx, (r, d)) = 
            let
                t = post (evx, r)
              in
                (t, d)

        swap_ :: ProcessA_ a ((b, d), p) (t, d)-> 
                 ProcessA_ a ((b, p), d) (t, d)
        swap_ (ProcessA_ pre post mc) = ProcessA_ (pre . sw) post mc
        sw ((b, d), p) = ((b, p), d)
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
