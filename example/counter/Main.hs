-- 参考：http://d.hatena.ne.jp/haxis_fx/20110726/1311657175

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module
    Main
where

import qualified Control.Arrow.Machine as P
import Control.Applicative ((<$>), (<*>))
import qualified Data.Machine as Mc
import qualified Control.Category as Cat
import Control.Arrow
import Control.Monad.State
import Control.Monad
import Control.Monad.Trans
import Debug.Trace

counter = 
    proc ev -> 
      do
        rec output <- returnA -< (\reset -> if reset then 0 else next) <$> ev
            next <- P.hold 0 <<< P.delay -< (+1) <$> output
        returnA -< output  

main = print $ P.runProcessA counter (map b "ffffffffttfftt")
  where b 't' = True
        b 'f' = False
