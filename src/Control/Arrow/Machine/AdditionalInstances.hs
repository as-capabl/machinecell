module
    Control.Arrow.Machine.AdditionalInstances ()
where

import Control.Arrow
import Control.Arrow.Machine.Types

instance
    Monad m => ArrowChoice (ProcessT m)
  where
    (|||) = undefined
    (+++) = undefined

instance
    Monad m => ArrowLoop (ProcessT m)
  where
    loop = undefined