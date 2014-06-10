{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NeuroSpider.Util.Gtk where

import Paths_NeuroSpider

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.String
import Graphics.UI.Gtk

class GObjectClass a => FromBuilder a where
  castTo :: GObject -> a
  getFrom :: Builder -> String -> IO a
  getFrom = flip builderGetObject castTo
  getListFrom :: Builder -> [String] -> IO [a]
  getListFrom = mapM . getFrom

instance FromBuilder a => IsString (Builder -> IO a) where
  fromString = flip getFrom

instance FromBuilder Button    where castTo = castToButton
instance FromBuilder Entry     where castTo = castToEntry
instance FromBuilder Editable  where castTo = castToEditable
instance FromBuilder Label     where castTo = castToLabel
instance FromBuilder TextBuffer     where castTo = castToTextBuffer
instance FromBuilder VButtonBox     where castTo = castToVButtonBox
instance FromBuilder ScrolledWindow where castTo = castToScrolledWindow

withBuilder :: FilePath -> (Builder -> IO a) -> IO Window
withBuilder bFile action = do
  builder <- getBuilder bFile
  window <- builderGetObject builder castToWindow "window1"
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

getBuilder :: FilePath -> IO Builder
getBuilder bFile = do
  builder <- builderNew
  builderAddFromFile builder =<< getDataFileName bFile
  return builder

fromBuilder :: Builder -> ReaderT Builder IO a -> IO a
fromBuilder b struct = runReaderT struct b

($->) :: (a -> b)
      -> (Builder -> IO a)
      -> ReaderT Builder IO b
f $-> x = f <$> ReaderT x

(*->) :: ReaderT Builder IO (a -> b)
      -> (Builder -> IO a)
      -> ReaderT Builder IO b
f *-> x = f <*> ReaderT x
