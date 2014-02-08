module NeuroSpider.Gui where

import NeuroSpider.Util.Gtk
import NeuroSpider.Util.GraphViz
--import NeuroSpider.Util.Reactive
--import NeuroSpider.Util.ReactiveGtk

import Prelude hiding (readFile)
import Data.Text.Lazy.IO (readFile)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView

runGUI :: IO ()
runGUI = doGUI $ withBuilder "main.glade" $ \builder -> do
  sw <- getFrom builder "scrolledwindow1" :: IO ScrolledWindow
  e <- getFrom builder "entry1" :: IO Entry
  wv <- webViewNew
  set sw [ containerChild := wv ]
  on e entryActivate $ do
    svg <- dotToSvg =<< readFile =<< entryGetText e
    webViewLoadString wv svg (Just "image/svg+xml") Nothing ""
