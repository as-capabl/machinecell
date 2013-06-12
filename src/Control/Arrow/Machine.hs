{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module
    Control.Arrow.Machine
where

import qualified Data.Machine as Mc
import qualified Control.Category as Cat
import Control.Monad (liftM)
import Control.Arrow
import Control.Applicative
import Control.Monad.Trans
import Debug.Trace

traceMc = const id
--traceMc = trace

data Event a = Event a | NoEvent

hEv :: ArrowApply a => a (e,b) c -> a e c -> a (e, Event b) c
hEv f1 f2 = proc (e, ev) ->
    helper ev -<< e
  where
    helper (Event x) = proc e -> f1 -< (e, x)
    helper NoEvent = f2

instance 
    Functor Event 
  where
    fmap f NoEvent = NoEvent
    fmap f (Event x) = Event (f x)

instance 
    Applicative Event 
  where
    pure = Event
    (Event f) <*> (Event x) = Event $ f x
    _ <*> _ = NoEvent



data ProcessA_ a b c =
    forall p q r. 
           ProcessA_ (b -> (Event p, r)) 
                     ((Event q, r) -> c)
                     (Mc.Machine (a p) q)

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

resolveCPS :: Arrow a => ProcessA a b c -> ProcessA_ a b c
resolveCPS (ProcessA cps) = dropIt $ cps $ ProcessA_ pre1 post1 mc1
  where
    pre1 (x, _) = (NoEvent, x)
    post1 = snd
    mc1 = Mc.Stop

    dropIt (ProcessA_ pre post mc) = ProcessA_ (pre . pad) post mc
    pad x = (x, undefined)


runProcessA_ (ProcessA_ pre post mc) =
    proc xs ->
      do
        (_, ys, _) <- runHelper (snd $ pre NoEvent) 
                      -< (map (pre . Event) xs, [], mc)
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
                hEv
                  (\bx -> do {bx' <- f -< bx; returnA -< (Event bx', r)})
                  (returnA -< (NoEvent, r))
                |)
                  (fst x)
            runHelper (snd ret) -<< (xs, ys, cont fc f ff (fst ret))
      where
        cont fc _ _ (Event x) = fc x
        cont fc f ff NoEvent = Mc.Await fc f ff


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
    post'' (ev1, ev2) c = post2 (ev2, (snd $ mid (ev1, c)))


type CM a c p q s= Mc.Machine (a (Event p, c)) (Event q, Event s)

concatenate' :: ArrowApply a => Mc.Machine (a p) q
             -> ((Event q, c) -> Event r)
             -> Mc.Machine (a r) s
             -> CM a c p q s

concatenate' Mc.Stop _ Mc.Stop = Mc.Stop

concatenate' mc1@(Mc.Await fc f ff) fmid mc2 = 
    traceMc "1:await" $
      Mc.Await id
          (proc (ev, c) -> 
            do
              (yields, mc1', mc2') <-
                (| 
                  hEv 
                    (\x ->
                      do
                        y <- f -< x
                        sweep1st (fc y) fmid mc2 -<< c)
                    --(yieldTo2nd fmid mc2 -<< (NoEvent, c))
                    (sweep1st mc1 fmid mc2 -<< c)
                  |) 
                    ev
              returnA -< yields $ concatenate' mc1' fmid mc2')
          (concatenate' ff fmid mc2)

concatenate' Mc.Stop fmid mc2 = 
    traceMc "1:stop" $
      Mc.Await id
        (proc (evx, c) ->
          do
            (| hEv
                (\x -> returnA -< Mc.Stop)
                (do
                  (yields, mc2') <- yieldTo2nd fmid mc2 -< (NoEvent, c)
                  returnA -< yields $ concatenate' Mc.Stop fmid mc2')
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

sweep1st mc1@(Mc.Yield x fc) fmid mc2 = 
    sweep1st' mc1 fmid mc2

sweep1st mc1 fmid mc2 = 
    traceMc "1:no yield" $ proc c ->
      do
        (yields, mc2') <- yieldTo2nd fmid mc2 -< (NoEvent, c)
        returnA -< (yields, mc1, mc2')

sweep1st' (Mc.Yield x fc) fmid Mc.Stop = 
    traceMc "1:yield" $ proc c ->
      do
        (| hEv
            (\y -> returnA -< (Mc.Yield (Event x, NoEvent), Mc.Stop, Mc.Stop))
            (cont -< c) 
          |)
            (fmid (Event x, c))
  where
    mc2 = Mc.Stop
    cont = proc c ->
      do
        (yields, mc2') <- yieldTo2nd fmid mc2 -< (Event x, c)
        (yieldsRest, mc1', mc2') <- sweep1st' fc fmid mc2' -<< c
        returnA -< (yields . yieldsRest, mc1', mc2')

sweep1st' (Mc.Yield x fc) fmid mc2 = 
    traceMc "1:yield" $ proc c ->
      do
        (yields, mc2') <- yieldTo2nd fmid mc2 -< (Event x, c)
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
        mc2' <-
          (|
            hEv
              (\x -> do {y <- g -< x; returnA -< gc y})
              (returnA -< mc2)
            |) 
              (fmid ex)

        -- 最初の一回だけev1はev2でない値を持ち得る
        sweep2nd fmid mc2' -<< (ex, (NoEvent, c))

yieldTo2nd fmid Mc.Stop = 
    traceMc "2:stop" $ proc (evx, c) ->
      returnA -< (Mc.Yield (evx, NoEvent), Mc.Stop)
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
