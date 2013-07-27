{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module
    Control.Arrow.Machine.Utils
where

import qualified Data.Machine as Mc
import Data.Machine ((~>))
import qualified Control.Category as Cat
import Control.Monad (liftM)
import Control.Arrow
import Control.Applicative
import Control.Monad.Trans
import Debug.Trace

import Control.Arrow.Machine.Types
import Control.Arrow.Machine.Event
import Control.Arrow.Machine.Detail


delay :: ArrowApply a => ProcessA a (Event b) (Event b)
delay = toProcessA $ Mc.construct $
    do
      x <- awaitA
      go x
  where
    go x = 
      do
        x2 <- awaitA
        Mc.yield x
        go x2

hold :: ArrowApply a => b -> ProcessA a (Event b) b
hold init = ProcessA $ holdImpl init


holdImpl :: ArrowApply a => b -> ProcessA_ a (b, d) t -> ProcessA_ a (Event b, d) t
holdImpl init (ProcessA_ pre post mc) = 
        ProcessA_ 
           (pre' pre) 
           (post' post) 
           (mc' pre init undefined mc)
  where
    -- pre' pre arg = (pre <$> ((,) <$> fst arg <*> Event (snd arg)) , ())
    pre' pre arg = 
        (Event arg, snd $ pre (evMaybe init id $ fst arg, snd arg))

    post' post (arg, r) = 
        evMaybe (post (NoEvent, r)) post arg -- 極めて怪しい

    mc' pre held _ (Mc.Stop) = 
        Mc.Stop
    mc' pre held r0 mc@(Mc.Await fc f ff) = 
        Mc.Await 
          id
          (proc (evx, d) ->
            do
              x <- arr (evMaybe held id) -< evx
              (evp, r) <- arr pre -< (x, d)
              (| hEv'
                  (\p -> do {y <- f -< p; returnA -< mc' pre x r (fc y)})
                  (returnA -< mc' pre held r0 mc)
                  (returnA -< Mc.Stop) --(returnA -< mc' pre held r0 mc)
                |)
                  evp)
          (mc' pre held r0 ff)
    mc' pre held r (Mc.Yield q fc) = 
        Mc.Yield (Event q, r) (mc' pre held r fc)
{-
        Mc.construct (holder pre x) ~> Mc.fit first mc
    holder pre xprev = 
      do
        (evx, d) <- Mc.await
        held <- case evx of 
          Event x -> x
          _ -> xprev
        Mc.yield $ pre (held, d)
        holder pre held
-}        
  
