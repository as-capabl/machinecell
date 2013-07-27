{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module
    Control.Arrow.Machine.Detail 
      (
         ProcessA_(..), 
         runProcessA_, 
         concatenate
      )
where

import qualified Data.Machine as Mc
import qualified Control.Category as Cat
import Control.Monad (liftM)
import Control.Arrow
import Control.Applicative
import Control.Monad.Trans
import Debug.Trace

import Control.Arrow.Machine.Event

traceMc = const id
--traceMc = trace


data ProcessA_ a b c =
    forall p q r. 
           ProcessA_ (b -> (Event p, r)) 
                     ((Event q, r) -> c)
                     (Mc.Machine (a p) q)



runProcessA_ (ProcessA_ pre post mc) =
    proc xs ->
      do
        (_, ys, _) <- runHelper (snd $ pre NoEvent) 
                      -< (map (pre . Event) xs ++ [pre End], [], mc)
        returnA -< ev2List $ map post ys
  where
    -- runHelper
    runHelper :: ArrowApply a => r 
                   -> a ([(Event b, r)], [(Event c, r)], Mc.Machine (a b) c)
                        ([(Event b, r)], [(Event c, r)], Mc.Machine (a b) c)

    runHelper r = proc (xs, ys, mc) -> 
      do
        transMc r mc -<< (xs, ys)

    -- ev2list
    ev2List ys = 
      do 
        y <- ys
        case y of 
          Event z -> [z]
          _ -> []

    -- transMc
    transMc :: ArrowApply a => r -> Mc.Machine (a b) c 
                 -> a ([(Event b, r)], [(Event c, r)]) 
                      ([(Event b, r)], [(Event c, r)], 
                       Mc.Machine (a b) c)

    transMc r Mc.Stop = 
        proc (xs, ys) -> go xs -<< ys
      where
        go ((NoEvent, r'):xs') = proc ys ->
          runHelper r' -< (xs', ys ++ [(NoEvent, r')], Mc.Stop)
        go xs = proc ys -> 
          returnA -< (xs, ys, Mc.Stop)

    transMc r (Mc.Yield y fc) = 
        proc (xs, ys) -> runHelper r -< (xs, ys ++ [(Event y, r)], fc)

    transMc r (Mc.Await fc f ff) = 
        proc (xxs, ys) -> awaitIt r xxs fc f ff -<< ys

    -- awaitIt
    awaitIt :: ArrowApply a => r -> [(Event b, r)]
                     -> (d -> Mc.Machine (a b) c)
                     -> a b d
                     -> Mc.Machine (a b) c
                     -> a [(Event c, r)] 
                          ([(Event b, r)], [(Event c, r)], Mc.Machine (a b) c)

    awaitIt r [] fc f ff = proc ys -> runHelper r -< ([], ys, ff)

    awaitIt _ (x:xs) fc f ff = 
        proc ys -> 
          do
            let r = snd x
            ret <- 
              (|
                hEv'
                  (\bx -> do {bx' <- f -< bx; returnA -< (Event bx', r)})
                  (returnA -< (NoEvent, r))
                  (returnA -< (End, r))
                |)
                  (fst x)
            let mcNext = cont fc f ff (fst ret)
            stepNext mcNext (snd ret) -<< (xs, ys, mcNext)
      where
        cont fc _ _ (Event x) = fc x
        cont fc f ff NoEvent = Mc.Await fc f ff
        cont _ _ ff End = ff

        -- なくていい
        stepNext Mc.Stop _ = returnA
        stepNext _ r = runHelper r




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

concatenate' _ _ Mc.Stop = Mc.Stop

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
                        sweep1st (fc y) fmid mc2 -<< c)
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
                  (yields, mc2') <- yieldTo2nd fmid mc2 -< (End, c)
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

sweep1st' :: ArrowApply a => Mc.Machine (a p) q
         -> ((Event q, c) -> Event r)
         -> Mc.Machine (a r) s
         -> a c (CM a c p q s -> CM a c p q s, 
                 Mc.Machine (a p) q, 
                 Mc.Machine (a r) s)

sweep1st _ _ Mc.Stop = 
    proc _ ->
      returnA -< (id, Mc.Stop, Mc.Stop)

sweep1st mc1@(Mc.Yield x fc) fmid mc2 = 
    sweep1st' mc1 fmid mc2


sweep1st Mc.Stop fmid mc2 = 
    proc c ->
      do
        (yields, mc2') <- yieldTo2nd fmid mc2 -< (End, c)
        returnA -< (yields, Mc.Stop, mc2')

sweep1st mc1 fmid mc2 = 
    traceMc "1:no yield" $ proc c ->
      do
        (yields, mc2') <- yieldTo2nd fmid mc2 -< (NoEvent, c)
        returnA -< (yields, mc1, mc2')

sweep1st' _ _ Mc.Stop = 
    proc _ ->
      returnA -< (id, Mc.Stop, Mc.Stop)
{-
sweep1st' (Mc.Yield x fc) fmid Mc.Stop = 
    traceMc "1:yield" $ proc c ->
      do
        (| hEv'
            (\y -> returnA -< (Mc.Yield (Event x, End), Mc.Stop, Mc.Stop))
            (cont -< c) 
            (returnA -< (Mc.Yield (Event x, End), Mc.Stop, Mc.Stop))
          |) -- TODO: 失敗でもsweep1stすべきでは？
            (fmid (Event x, c))
  where
    mc2 = Mc.Stop
    cont = proc c ->
      do
        (yields, mc2') <- yieldTo2nd fmid mc2 -< (Event x, c)
        (yieldsRest, mc1', mc2') <- sweep1st' fc fmid mc2' -<< c
        returnA -< (yields . yieldsRest, mc1', mc2')
-}
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

sweep1st' mc1 fmid mc2 = 
    proc c ->
      do
        returnA -< (id, mc1, mc2)

yieldTo2nd :: ArrowApply a => ((Event q, c) -> Event r)
           -> Mc.Machine (a r) s
           -> a (Event q, c) 
                (CM a c p q s -> CM a c p q s, Mc.Machine (a r) s)

yieldTo2nd fmid mc2@(Mc.Await gc g gf) = 
    traceMc "2:await" $ proc ex@(_, c) ->
      do
        (|
          hEv'
            (\x -> 
              do 
                y <- g -< x
                sweep2nd fmid (gc y)-<< (ex, (NoEvent, c)))
            (sweep2nd fmid mc2 -<< (ex, (NoEvent, c)))
            (returnA -< (Mc.Yield (End, End), gf))
          |) 
            (fmid ex)

        -- 最初の一回だけev1はev2でない値を持ち得る
        

yieldTo2nd fmid Mc.Stop = 
    traceMc "2:stop" $ proc (evx, c) ->
      returnA -< (Mc.Yield (evx, End), Mc.Stop)
--      returnA -< (id, Mc.Stop)
      

yieldTo2nd fmid (Mc.Yield _ _) = undefined

sweep2nd fmid (Mc.Yield y gc) = 
    traceMc "2:yield" $ proc (evs1, evs2) ->
      do
        let ev1 = fmid evs1
        let ev2 = fmid evs2

        (yields, mc2) <- sweep2nd fmid gc -< (evs2, evs2)
        returnA -< (Mc.Yield (fst evs1, Event y) . yields, mc2)

sweep2nd fmid mc2 =
    proc _ ->
        returnA -< (id, mc2)
