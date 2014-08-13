{-# LANGUAGE RankNTypes #-}

module NeuroSpider.Util.Reactive (run) where

import BasicPrelude
import Reactive.Banana
import Reactive.Banana.Frameworks

type Moment' a = Frameworks t => Moment t a

run :: Moment' () -> IO ()
run = actuate <=< compile

