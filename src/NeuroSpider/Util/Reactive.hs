{-# LANGUAGE RankNTypes #-}

module NeuroSpider.Util.Reactive where

import Control.Monad
import Reactive.Banana
import Reactive.Banana.Frameworks

type Moment' a = Frameworks t => Moment t a

newAddHandlers :: Int -> IO ([AddHandler a], [a -> IO ()])
newAddHandlers = liftM unzip . flip replicateM newAddHandler

run :: Moment' () -> IO ()
run = actuate <=< compile

foldE :: (a -> b -> b) -> b -> Event t a -> Event t b
foldE f acc = accumE acc . fmap f
