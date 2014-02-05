module NeuroSpider.Util.ReactiveGtk where

import Control.Monad.Reader
import Graphics.UI.Gtk

sendEvent :: (b -> IO ()) -> EventM a b -> EventM a Bool
sendEvent f = mapReaderT (\e -> e >>= f >> return False)

send :: a -> (a -> IO ()) -> EventM b Bool
send x f = sendEvent f (return x)

fire :: (() -> IO ()) -> EventM a Bool
fire = send ()
