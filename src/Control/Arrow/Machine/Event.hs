module
    Control.Arrow.Machine.Event 
      (
        Occasional (..),
        Event (),

        -- * Deprecated
        -- |They should be used only for internal use.
        hEv, 
        hEv', 
        evMaybe,
        fromEvent,
        evMap,
        split,
        join,
        split2,
        join2
      )
where

import Control.Arrow.Machine.Event.Internal
