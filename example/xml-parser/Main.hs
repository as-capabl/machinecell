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
import Control.Arrow.Machine.ArrowUtil
import qualified Control.Arrow.Machine.Utils as Mc
import ConduitAdaptor


--
-- Local definitions
--
type MainMonad = ResourceT IO
type MainArrow = Kleisli MainMonad

liftConduit ::
    Cd.ConduitM i o MainMonad r -> ProcessA MainArrow (Event i) (Event o)
liftConduit = constructAuto (^.uc0.kl)

io :: MonadIO m => Getter (a -> IO b) (a -> m b)
io = to (liftIO .)

makePrisms ''XML.Event
forkOf ::
    ArrowApply a =>
    Fold s b -> ProcessA a (Event s) (Event b)
forkOf fd = repeatedly $ await >>= mapMOf_ fd yield

--
-- Program
--
mainArrow file = proc start ->
  do
    -- File read
    source <- liftConduit $ CE.sourceFile file -< start

    -- XML Parse
    parseEvents <- liftConduit $ XML.parseBytes XML.def -< source

    -- Remember depth
    beginElem <- forkOf _EventBeginElement -< parseEvents
    endElem <- forkOf _EventEndElement -< parseEvents
    depth <- Mc.dAccum (0::Int) <<< Mc.gather -< [(+1) <$ beginElem, (\x->x-1) <$ endElem]
    
    -- output tag name at the depth
    let tagName = XML.nameLocalName . fst <$> beginElem
    Mc.anytime $ Txt.putStrLn ^.io.kl -<
        (Txt.pack (join $ replicate depth "  ") `mappend`) <$> tagName

main =
  do
    args <- getArgs
    runResourceT $ kl # run (mainArrow $ head args) $ []
