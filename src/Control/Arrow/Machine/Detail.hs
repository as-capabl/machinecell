{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module
    Control.Arrow.Machine.Detail 
      (
{-         ProcessA_(..), 
         runProcessA_, 
         concatenate,
         feedTo,
         loopProcessA_-}
      )
where

import qualified Control.Category as Cat
import Control.Monad (liftM)
import Control.Arrow
import Control.Applicative
import Control.Monad.Trans
import Debug.Trace

import Control.Arrow.Machine.Event

{-
traceMc = const id
--traceMc = trace


data ProcessA_ a b c =
    forall p q r. 
           ProcessA_ (b -> (Event p, r)) 
                     ((Event q, r) -> c)
                     (Mc.Machine (a p) q)



runProcessA_  pa =
    proc xs ->
      do
        let newPa = concatenate (source xs) pa
        go2 newPa -<< []
  where
    go2 (ProcessA_ pre post mc) = proc _ ->
      do
        ys <- go (snd $ pre NoEvent) mc (map pre [Event (), NoEvent]) -< []
        returnA -< ev2List $ map post ys
    
    go r mc [] = proc ys ->
      do
        (l, _) <- feedTo id mc -< End
        l' <- returnA -<  zip l (repeat r)
        returnA -< ys ++ l'

    go r mc ((x,r'):xs) = proc ys ->
      do
        (l, mc2) <- feedTo id mc -< x
        l' <- returnA -< zip l ([r'] ++ repeat r)
        go r mc2 xs -<< ys ++ l'

    -- ev2list
    ev2List ys = 
      do 
        y <- ys
        case y of 
          Event z -> [z]
          _ -> []

    source xs = ProcessA_ pre post mc
      where
        pre = \evx -> (evx, ())
        post = fromEvent NoEvent . fst -- TODO: NoEvent不要なら排除
        mc = sourceMc xs

    sourceMc xs = Mc.construct $
      do
        _ <- Mc.request Cat.id
        mapM (\x -> Mc.yield (Event x)) xs


-- concatenate
--   内部処理はsweep1st -> yieldTo2nd -> sweep2nd
concatenate (ProcessA_ pre1 post1 mc1) (ProcessA_ pre2 post2 mc2) = 
    ProcessA_ pre' post' (concatenate' mc1 (fst . mid) mc2)
  where
    mid = pre2 . post1
    pre' x =  
      let
          evy@(ev, y) = pre1 x
        in
          (Event evy, y) 
          -- case ev of {Event _ -> (Event evy, y); NoEvent -> (NoEvent, y) }
    post' (Event qs, c) = post'' qs c
    post' (NoEvent, c) = post'' (NoEvent, NoEvent) c
    post' (End, c) = post'' (End, End) c

    post'' (ev1, ev2) c = post2 (ev2, (snd $ mid (ev1, c)))


type CM a c p q s= Mc.Machine (a (Event p, c)) (Event q, Event s)

concatenate' :: ArrowApply a => Mc.Machine (a p) q
             -> ((Event q, c) -> Event r)
             -> Mc.Machine (a r) s
             -> CM a c p q s

-- concatenate' _ _ Mc.Stop = Mc.Stop

concatenate' mc1@(Mc.Await fc f ff) fmid mc2 = 
    traceMc "1:await" $
      Mc.Await id
          (proc (evx, c) -> 
            do
              (yields, mc1', mc2') <-
                (| 
                  hEv' 
                    (\x ->
                      do
                        y <- f -< x
                        (sweep1st (fc y) fmid mc2 -<< c))
                    (sweep1st mc1 fmid mc2 -<< c)
                    (returnA -< (id, ff, mc2))
                  |) 
                    evx
              returnA -< yields $ concatenate' mc1' fmid mc2')
          (concatenate' ff fmid mc2)

concatenate' Mc.Stop fmid mc2 = 
    traceMc "1:stop" $
      Mc.Await id
        (proc (evx, c) ->
          do
            (| hEv'
                (\x -> returnA -< Mc.Stop)
                (do
                  (yields, mc2') <- yieldTo2nd fmid mc2 -< (NoEvent, c)
                  returnA -< yields $ concatenate' Mc.Stop fmid mc2')
                (returnA -< Mc.Stop)
              |)
                evx)
        Mc.Stop


sweep1st :: ArrowApply a => Mc.Machine (a p) q
         -> ((Event q, c) -> Event r)
         -> Mc.Machine (a r) s
         -> a c (CM a c p q s -> CM a c p q s, 
                 Mc.Machine (a p) q, 
                 Mc.Machine (a r) s)

sweep1st mc1@(Mc.Yield x fc) fmid mc2 = 
    sweep1st' mc1 fmid mc2

sweep1st mc1 fmid mc2 = 
    traceMc "1:no yield" $ proc c ->
      do
        (yields, mc2') <- yieldTo2nd fmid mc2 -< (NoEvent, c)
        returnA -< (yields, mc1, mc2')

sweep1st' :: ArrowApply a => Mc.Machine (a p) q
         -> ((Event q, c) -> Event r)
         -> Mc.Machine (a r) s
         -> a c (CM a c p q s -> CM a c p q s, 
                 Mc.Machine (a p) q, 
                 Mc.Machine (a r) s)

sweep1st' (Mc.Yield x fc) fmid mc2 = 
    traceMc "1:yield" $ proc c ->
      do
        (yields, mc2') <- yieldTo2nd fmid mc2 -< (Event x, c)
        cont mc2' yields -<< c
  where
    cont Mc.Stop yields = proc c ->
      returnA -< (yields, Mc.Stop, Mc.Stop)

    cont mc2' yields = proc c ->
      do
        (yieldsRest, mc1', mc2') <- sweep1st' fc fmid mc2' -<< c
        returnA -< (yields . yieldsRest, mc1', mc2')

sweep1st' mc1 _ mc2 = proc _ -> returnA -< (id, mc1, mc2)


yieldTo2nd :: ArrowApply a => ((Event q, c) -> Event r)
           -> Mc.Machine (a r) s
           -> a (Event q, c) 
                (CM a c p q s -> CM a c p q s, Mc.Machine (a r) s)

yieldTo2nd fmid mc2 = proc evqc -> 
  do 
    let
        (evq, r) = evqc
        fmid' evx = fmid (evx, r)
    (ys, nextMc) <- feedTo fmid' mc2 -<< evq
    returnA -< (listToMc evq ys, nextMc)
  where
    listToMc evq (End:_) = Mc.Yield (evq, End) . const Mc.Stop
    listToMc evq (x:xs) = Mc.Yield (evq, x) . listToMc NoEvent xs
    listToMc NoEvent [] = id
    listToMc evr [] = Mc.Yield (evr, NoEvent)



feedTo :: ArrowApply a => (Event q -> Event r)
           -> Mc.Machine (a r) s
           -> a (Event q) 
                ([Event s], Mc.Machine (a r) s)

feedTo fmid mc2@(Mc.Await gc g gf) = 
    traceMc "2:await" $ proc evx ->
      do
        (|
          hEv'
            (\x -> 
              do 
                y <- g -< x
                sweep2nd fmid (gc y)-<< ())
            (sweep2nd fmid mc2 -<< ())
            (returnA -< ([End], gf))
          |) 
            (fmid evx)
        

feedTo fmid Mc.Stop = 
    traceMc "2:stop" $ proc evx ->
      do
        (| hEv'
           (\_ -> returnA -< ([End], Mc.Stop))
           (returnA -< ([], Mc.Stop))
           (returnA -< ([End], Mc.Stop))
          |)
           evx
--      returnA -< (id, Mc.Stop)
      

feedTo fmid (Mc.Yield _ _) = undefined

sweep2nd fmid (Mc.Yield y gc) = 
    traceMc "2:yield" $ proc () ->
      do
        (yields, mc2) <- sweep2nd fmid gc -< ()
        returnA -< ([Event y] ++ yields, mc2)

sweep2nd fmid Mc.Stop = 
    traceMc "2:yield" $ proc () ->
      do
        returnA -< ([End], Mc.Stop)

sweep2nd fmid mc2 =
    proc _ ->
        returnA -< ([], mc2)



--
-- ArrowLoop
--
loopProcessA_ ::
    (ArrowApply a, ArrowLoop a) =>
    ProcessA_ a (b, d) (c, d) -> ProcessA_ a b c

loopProcessA_ (ProcessA_ pre post Mc.Stop) =
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

loopProcessA_ (ProcessA_ pre post mc) =
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
-}
