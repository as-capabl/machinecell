{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module
    Automaton (
        Automaton(..),
        constructAuto
      )
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Arrow.Machine.Types
import Control.Arrow.Machine.ArrowUtil
import qualified Control.Arrow.Machine.Utils as Mc

class
    Automaton m p q a | a -> m, a -> p, a -> q
  where
    auto :: a -> PlanT p q m r

constructAuto ::
    (Automaton m i o a, Monad m) =>
    a ->
    ProcessT m (Event i) (Event o)
constructAuto = constructT . auto

