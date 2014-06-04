{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NeuroSpider.Gui where

import Paths_NeuroSpider

import NeuroSpider.Graph
import NeuroSpider.Util.Gtk
import NeuroSpider.Util.GraphViz
import NeuroSpider.Util.XML
import NeuroSpider.Util.Reactive
--import NeuroSpider.Util.ReactiveGtk
import Data.Graph.Inductive.Example (vor)
import Data.Graph.Inductive.Graph (nodeRange, insNode)
--import Data.String

import Prelude hiding (lookup, mapM)
import Control.Monad (forM_)
import Data.Default
import Data.Map (Map, (!), fromList, lookup, insert, keys, elems, size)
import Data.Text.Lazy (Text, unpack)
import Data.Traversable (mapM)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Reactive.Banana
import Reactive.Banana.Frameworks
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
  (hs, es) <- newAddHandlers $ size buttons
  (navHandler, navEvent) <- newAddHandler
  (labelHandler, labelEvent) <- newAddHandler
  run $ do
    nav <- fromAddHandler navHandler
    label <- fromChanges def labelHandler
    button <- fromList . zip (keys buttons) <$> mapM fromAddHandler hs
    let (_, click) = split $ parseGraphEvent <$> nav
    let select = gElem <$> click
    let selects = accumE def $ (\n (s, _) -> (Just n, s)) <$> select
    let select2 = filterJust $ snd <$> selects
    let selected = stepper def select
    let graph = accumE graphInit $ unions [del, ins]
          where
            del = delElem <$> selected <@ button ! "deleteSelected"
            ins = union createNode createEdge
            createNode = insNode <$> ((,) <$> newNode <*> label)
                         <@ button ! "createNode"
            createEdge = makeEdge
                           <$> stepper def select2
                           <*> selected
                           <*> fmap (readDef def) label
                         <@ button ! "createEdge"
            newNode = accumB newNodeStart $ pure (+1) <@ createNode
    reactimate $ loadWv wv <$> graph
    reactimate $ print <$> label <@ button ! "renameSelected"
    reactimate $ textBufferSetText tb1 . show <$> select
    reactimate $ textBufferSetText tb2 . show <$> select2
  _ <- on wv navigationPolicyDecisionRequested $ \_ request _ _ -> do
    uri <- networkRequestGetUri request
    case uri of
      Nothing -> return False
      Just u  -> navEvent u >> return True
  _ <- on e2 editableChanged $ labelEvent =<< get e2 entryText
  forM_ (zip (elems buttons) es) $ \(b, e) -> on b buttonActivated (e ())
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
