{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}


{-|
Extracted stuff to be used unqualified from ../Machine.hs.
-}
module
    Control.Arrow.Machine.Core
      (
        -- * Modules
        module Control.Arrow.Machine.Event, 
--        module Control.Arrow.Machine.Utils,
        module Control.Arrow.Machine.Plan,
--        module Control.Arrow.Machine.Exception,
        module Control.Arrow.Machine.Running,
--        module Control.Arrow.Machine.ArrowUtil,

        -- * The transducer arrow
        ProcessA(), 
        fit,
       )
where

import Control.Arrow.Machine.Event
-- import Control.Arrow.Machine.Utils
import Control.Arrow.Machine.Plan
-- import Control.Arrow.Machine.Exception
import Control.Arrow.Machine.Running
import qualified Control.Arrow.Machine.Running as Running
-- import Control.Arrow.Machine.ArrowUtil

import Control.Arrow.Machine.Types
import Control.Arrow


