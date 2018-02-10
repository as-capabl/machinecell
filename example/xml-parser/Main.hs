{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad

import Control.Lens
import qualified Data.Conduit as Cd
import qualified Data.XML.Types as XML
import qualified Text.XML.Stream.Parse as XML
import qualified Data.Conduit.Binary as CE
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import System.Environment (getArgs)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class

import Control.Arrow.Machine.Types
import qualified Control.Arrow.Machine.Utils as Mc
import ConduitAdaptor


--
-- Local definitions
--
makePrisms ''XML.Event
forkOf ::
    Monad m =>
    Fold s b -> ProcessT m (Event s) (Event b)
forkOf fd = repeatedly $ await >>= mapMOf_ fd yield

--
-- Program
--
mainArrow file = proc start ->
  do
    -- File read
    source <- constructAuto $ CE.sourceFile file -< start

    -- XML Parse
    parseEvents <- constructAuto $ XML.parseBytes XML.def -< source

    -- Remember depth
    beginElem <- forkOf _EventBeginElement -< parseEvents
    endElem <- forkOf _EventEndElement -< parseEvents
    depth <- Mc.dAccum (0::Int) <<< Mc.gather -< [(+1) <$ beginElem, (\x->x-1) <$ endElem]

    -- output tag name at the depth
    let tagName = XML.nameLocalName . fst <$> beginElem
    Mc.fire $ liftIO . Txt.putStrLn -<
        (Txt.pack (join $ replicate depth "  ") `mappend`) <$> tagName

main =
  do
    args <- getArgs
    runResourceT $ runT_ (mainArrow $ head args) []
