module NeuroSpider.Util.Gtk where

import Paths_NeuroSpider

import Control.Monad.IO.Class
import Graphics.UI.Gtk

class GObjectClass a => FromBuilder a where
  castTo :: GObject -> a
  getFrom :: Builder -> String -> IO a
  getFrom = flip builderGetObject castTo
  getListFrom :: Builder -> [String] -> IO [a]
  getListFrom = mapM . getFrom

instance FromBuilder Button    where castTo = castToButton
instance FromBuilder Entry     where castTo = castToEntry
instance FromBuilder Editable  where castTo = castToEditable
instance FromBuilder Label     where castTo = castToLabel
instance FromBuilder TextBuffer     where castTo = castToTextBuffer
instance FromBuilder VButtonBox     where castTo = castToVButtonBox
instance FromBuilder ScrolledWindow where castTo = castToScrolledWindow

withBuilder :: FilePath -> (Builder -> IO a) -> IO Window
withBuilder bFile action = do
  builder <- builderNew
  builderAddFromFile builder =<< getDataFileName bFile
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
