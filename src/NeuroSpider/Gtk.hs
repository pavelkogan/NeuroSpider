{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NeuroSpider.Gtk
  ( withBuilder
  , fromBuilder
  , doGUI
  ) where

import NeuroSpider.Paths

import BasicPrelude hiding (on)
import Control.Monad.Trans.Reader
import Graphics.UI.Gtk

newtype GetFromBuilder a = GetFromBuilder
  { getFromBuilder :: ReaderT Builder IO a
  } deriving (Functor, Applicative, Monad, MonadIO)

instance FromBuilder a => IsString (GetFromBuilder a) where
  fromString = GetFromBuilder . ReaderT . flip getFrom

fromBuilder :: Builder -> GetFromBuilder a -> IO a
fromBuilder = flip $ runReaderT . getFromBuilder

class GObjectClass a => FromBuilder a where
  castTo :: GObject -> a
  getFrom :: Builder -> String -> IO a
  getFrom = flip builderGetObject castTo

instance FromBuilder Button    where castTo = castToButton
instance FromBuilder Entry     where castTo = castToEntry
instance FromBuilder Editable  where castTo = castToEditable
instance FromBuilder Label     where castTo = castToLabel
instance FromBuilder VBox      where castTo = castToVBox
instance FromBuilder Window    where castTo = castToWindow
instance FromBuilder TextBuffer     where castTo = castToTextBuffer
instance FromBuilder VButtonBox     where castTo = castToVButtonBox
instance FromBuilder ScrolledWindow where castTo = castToScrolledWindow

withBuilder :: String -> (Builder -> IO a) -> IO Window
withBuilder bFile action = do
  builder <- getBuilder bFile
  window <- getFrom builder "window1"
  _ <- action builder
  return window

doGUI :: IO Window -> IO ()
doGUI action = do
  _ <- initGUI
  window <- action
  widgetShowAll window
  _ <- on window deleteEvent $ perform mainQuit
  mainGUI

perform :: MonadIO m => IO a -> m Bool
perform action = liftIO action >> return False

getBuilder :: String -> IO Builder
getBuilder bFile = do
  builder <- builderNew
  bString <- readFile =<< getDataFileName bFile
  builderAddFromString builder bString
  return builder

