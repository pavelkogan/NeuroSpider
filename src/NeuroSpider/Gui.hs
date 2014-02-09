{-# LANGUAGE OverloadedStrings #-}

module NeuroSpider.Gui where

import Paths_NeuroSpider

import NeuroSpider.Util.Gtk
import NeuroSpider.Util.GraphViz
--import NeuroSpider.Util.Reactive
--import NeuroSpider.Util.ReactiveGtk

import Control.Monad
import Data.Conduit
import Data.Monoid
import Data.Text
import Data.XML.Types
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Text.XML.Stream.Parse
import Text.XML.Stream.Render
import qualified Data.Conduit.List as CL
import qualified Data.Text.IO as T

runGUI :: IO ()
runGUI = doGUI $ withBuilder "main.glade" $ \builder -> do
  sw <- getFrom builder "scrolledwindow1" :: IO ScrolledWindow
  e <- getFrom builder "entry1" :: IO Entry
  wv <- webViewNew
  set sw [ containerChild := wv ]
  on e entryActivate $ do
    xml <- (dotToSvg =<< readFile =<< entryGetText e) :: IO Text
    svg <- augmentSvg xml
    webViewLoadString wv (unpack svg) (Just "image/svg+xml") Nothing ""

augmentSvg :: Text -> IO Text
augmentSvg i = liftM mconcat $
     yield i
  $$ parseText def
  =$ CL.concatMapM (addCss . snd)
  =$ renderText def
  =$ CL.consume

addCss :: Event -> IO [Event]
addCss e@(EventBeginElement (Name "svg" _ _) _) = do
  let begin = EventBeginElement "{http://www.w3.org/2000/svg}style"
                                [("type", [ContentText "text/css"])]
      end   = EventEndElement "style"
  css <- liftM EventCDATA $ T.readFile =<< getDataFileName "main.css"
  return [e, begin, css, end]
addCss e = return [e]
