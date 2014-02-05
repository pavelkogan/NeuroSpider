module NeuroSpider.Util.Gtk where

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
instance FromBuilder ScrolledWindow where castTo = castToScrolledWindow

withBuilder :: FilePath -> (Builder -> IO a) -> IO Window
withBuilder bFile action = do
  builder <- builderNew
  builderAddFromFile builder bFile
  window <- builderGetObject builder castToWindow "window1"
  _ <- action builder
  return window

doGUI :: IO Window -> IO ()
doGUI action = do
  initGUI
  window <- action
  widgetShowAll window
  on window deleteEvent $ perform mainQuit
  mainGUI

perform :: MonadIO m => IO a -> m Bool
perform action = liftIO action >> return False
