{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module
    Control.Arrow.Machine.Misc.Pump
      (
        intake,
        outlet
      )
where

import Prelude hiding (id, (.))
import Data.Functor
import Control.Category
import Control.Arrow
import qualified Control.Arrow.Machine as P
import Data.Monoid (Endo(Endo), mappend, appEndo)

newtype Duct a = Duct { unDuct :: Endo [a] }

oneMore ::
    ArrowApply a =>
    P.ProcessA a (P.Event ()) (P.Event ())
oneMore = proc ev ->
  do
    ed <- P.onEnd -< ev
    P.gather -< [ev, ed]
    
intake ::
    ArrowApply a =>
    P.ProcessA a (P.Event b, P.Event ()) (Duct b)
intake = proc (ev, clock) ->
  do
    cl2 <- oneMore -< clock
    append <- returnA -< (\x y -> y `mappend` Endo (x:)) <$> ev
    e <- P.accum (Endo id) <<< P.gather -< [ (const $ Endo id) <$ cl2, append ]
    returnA -< Duct e

outlet ::
    ArrowApply a =>
    P.ProcessA a (Duct b, P.Event ()) (P.Event b)
outlet = proc (~(Duct dct), clock) ->
  do
    cl2 <- oneMore -< clock
    dct' <- P.cycleDelay -< dct
    P.fork -< appEndo dct' [] <$ cl2

