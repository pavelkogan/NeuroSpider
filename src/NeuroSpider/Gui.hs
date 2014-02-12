{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NeuroSpider.Gui where

import Paths_NeuroSpider

import NeuroSpider.Util.Gtk
import NeuroSpider.Util.GraphViz
import NeuroSpider.Util.XML
--import NeuroSpider.Util.Reactive
--import NeuroSpider.Util.ReactiveGtk

import Data.Map
import Data.Text.Lazy
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Text.XML hiding (readFile)
import qualified Data.Text.Lazy.IO as T

runGUI :: IO ()
runGUI = doGUI $ withBuilder "main.glade" $ \builder -> do
  sw <- getFrom builder "scrolledwindow1" :: IO ScrolledWindow
  e <- getFrom builder "entry1" :: IO Entry
  wv <- webViewNew
  set sw [ containerChild := wv ]
  on e entryActivate $ do
    xml <- (dotToSvg =<< readFile =<< entryGetText e) :: IO Text
    css <- T.readFile =<< getDataFileName "main.css"
    js <- T.readFile =<< getDataFileName "main.js"
    let svg = renderText def $ transformSvg (parseText_ def xml) css js
    webViewLoadString wv (unpack svg) (Just "image/svg+xml") Nothing ""

transformSvg :: Document -> Text -> Text -> Document
transformSvg d@(Document{..}) css js = d{documentRoot = documentRoot'}
  where
    documentRoot' = goElem documentRoot
    goElem e@(Element{..}) = case elementName of
      Name{nameLocalName = "svg",..} -> e{elementNodes = script++style++nodes}
      Name{nameLocalName = "g"  ,..} ->
        let newAttrs = addClick elementAttributes
        in  e{elementNodes = nodes, elementAttributes = newAttrs}
      _                              -> e{elementNodes = nodes}
      where nodes  = fmap goNode elementNodes
            style  = svgNodes "style"  css
            script = svgNodes "script" js
    goNode (NodeElement e) = NodeElement $ goElem e
    goNode n               = n
    addClick attrs = if isSubmapOf (fromList [("class","node")]) attrs
      then insert "onclick" "clickHandler(this)" attrs else attrs

