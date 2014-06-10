{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NeuroSpider.Gui where

import Paths_NeuroSpider

import NeuroSpider.Graph
import NeuroSpider.Util.Gtk
import NeuroSpider.Util.GraphViz
import NeuroSpider.Util.XML
import NeuroSpider.Util.Reactive
import Data.Graph.Inductive.Example (vor)
import Data.Graph.Inductive.Graph (nodeRange, insNode)
--import Data.String

import Prelude hiding (lookup, mapM)
import Data.Default
import Data.Map (Map, (!), fromList, lookup, insert, elems)
import Data.Text.Lazy (Text, unpack)
import Data.Traversable (mapM)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Gtk
import Safe (readDef)
import Text.XML hiding (readFile)
import qualified Data.Text.Lazy.IO as T

runGUI :: IO ()
runGUI = doGUI $ withBuilder "main.glade" $ \builder -> do
  let graphInit = vor
  let newNodeStart = (+1) . snd . nodeRange $ graphInit
  sw <- getFrom builder "scrolledwindow1" :: IO ScrolledWindow
  --[e1,e2] <- getListFrom builder ["entry1","entry2"] :: IO [Entry]
  e2 <- getFrom builder "entry2" :: IO Entry
  tb1 <- getFrom builder "textbuffer1" :: IO TextBuffer
  tb2 <- getFrom builder "textbuffer2" :: IO TextBuffer
  bb <- getFrom builder "vbuttonbox1" :: IO VButtonBox
  wv <- webViewNew
  set sw [ containerChild := wv ]
  buttons <- makeButtons
  mapM_ (containerAdd bb) $ elems buttons
  run $ do
    nav <- eventN (\f _ request _ _ -> do
      uri <- networkRequestGetUri request
      case uri of
        Nothing -> return False
        Just u  -> f u >> return True
      ) wv navigationPolicyDecisionRequested
    label <- stepper def <$> monitorAttr e2 editableChanged entryText
    button <- mapM (flip event0 buttonActivated) buttons
    let (_, click) = split $ parseGraphEvent <$> nav
    let select = gElem <$> click
    let selects = accumE def $ (\n (s, _) -> (Just n, s)) <$> select
    let selected = stepper def select
    let selected2 = stepper def $ filterJust $ snd <$> selects
    let graph = accumE graphInit $ unions [del, ins, lab]
          where
            del = delElem <$> selected <@ button ! "deleteSelected"
            ins = union createNode createEdge
            lab = labelElem <$> label <*> selected <@ button ! "renameSelected"
            createNode = insNode <$> ((,) <$> newNode <*> label)
                         <@ button ! "createNode"
            createEdge = makeEdge
                           <$> selected2
                           <*> selected
                           <*> fmap (readDef def) label
                         <@ button ! "createEdge"
            newNode = accumB newNodeStart $ pure (+1) <@ createNode
    reactimate $ loadWv wv <$> graph
    sink tb1 [textBufferText :== show <$> selected]
    sink tb2 [textBufferText :== show <$> selected2]
  loadWv wv graphInit
  where
  loadWv wv x = do
    xml <- graphToSvg x :: IO Text
    css <- T.readFile =<< getDataFileName "main.css"
    js <- T.readFile =<< getDataFileName "main.js"
    let svg = renderText def $ transformSvg (parseText_ def xml) css js
    webViewLoadString wv (unpack svg) (Just "image/svg+xml") Nothing ""

makeButtons :: IO (Map String Button)
makeButtons = mapM buttonNewWithLabel $ fromList
  [ ("createNode", "Create Node")
  , ("createEdge", "Create Edge")
  , ("deleteSelected", "Delete Selected")
  , ("renameSelected", "Rename Selected") ]

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
    addClick attrs = case lookup "class" attrs of
      Just a  -> if any (==a) ["node", "edge"]
                   then insert "onclick" "clickHandler(this)" attrs
                   else attrs
      Nothing -> attrs
