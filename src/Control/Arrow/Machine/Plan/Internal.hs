{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module
    Control.Arrow.Machine.Plan.Internal
where

import qualified Control.Monad.Trans.Free as F
import qualified Control.Monad.Trans.Free.Church as F

data PlanF i o a where
  AwaitPF :: (i->a) -> a -> PlanF i o a
  YieldPF :: o -> a -> PlanF i o a
  StopPF :: PlanF i o a

instance (Functor (PlanF i o)) where
  fmap g (AwaitPF f ff) = AwaitPF (g . f) (g ff)
  fmap g (YieldPF x r) = YieldPF x (g r)
  fmap g StopPF = StopPF


type PlanT i o m a = F.FT (PlanF i o) m a
type Plan i o a = forall m. Monad m => PlanT i o m a
