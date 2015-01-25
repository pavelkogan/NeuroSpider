{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module NeuroSpider.Gui (runGUI) where

import NeuroSpider.Graph
import NeuroSpider.GraphViz
import NeuroSpider.Gtk
import NeuroSpider.Paths
import NeuroSpider.UiManager
import NeuroSpider.XML

import Data.Graph.Inductive.Graph (nodeRange, insNode)
import qualified Data.Graph.Inductive.Graph as Graph (empty)
import qualified Data.Graph.Inductive.Tree as Graph

import BasicPrelude hiding (mapM, union, on)
import Data.Default.Generics
import Data.Map ((!), fromList)
import Data.Traversable (mapM)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Gtk
import qualified Filesystem.Path.CurrentOS as Filepath

data Widgets = Widgets
                 Entry
                 TextBuffer
                 TextBuffer
                 WebView
                 FileChooserDialog
                 (Map UiAction Action)

runGUI :: IO ()
runGUI = doGUI $ withBuilder "main.glade" $ \builder -> do
  widgets <- setUpGUI builder
  network <- compile $ eventNetwork widgets
  actuate network

eventNetwork :: forall t. Frameworks t => Widgets -> Moment t ()
eventNetwork (Widgets e1 tb1 tb2 wv od actions) = do
  nav <- eventN (\f _ request _ _ -> do
    uri <- networkRequestGetUri request
    case uri of
      Nothing -> return False
      Just u  -> f u >> return True
    ) wv navigationPolicyDecisionRequested
  action <- mapM (`event0` actionActivated) actions
  fileChoice <- fmap filterJust $ monitorG od response $ flip $ \case
    ResponseAccept -> \c -> do
      f <- fileChooserGetFilename c
      a <- fileChooserGetAction c
      return $ (a,) <$> f
    _              -> const $ return Nothing
  fileString <- fmap filterJust $ monitorG od response $ flip $ \case
    ResponseAccept -> fileChooserGetFilename
    _              -> const $ return Nothing
  labelString <- monitorAttr e1 editableChanged entryText
  let loadFileE = Filepath.fromText . fromString <$> fileString
  let file = stepper "" loadFileE
  let label = fromString <$> stepper def labelString
  let (_, click) = split $ parseGraphEvent <$> nav
  let select = gElem <$> click
  let selected = stepper def select
  let selected2 = stepper def $ selected <@ select

  let loadGraphE = filterJust $ maybeRead <$> loadFileE
  (graphString, loadGraph) <- newEvent
  reactimate $ (loadGraph =<<) <$> loadGraphE
  let loadedGraph = (readGraph :: Text -> Graph.Gr Text Text) <$> graphString
  let newNodeStart = (+1) . snd . nodeRange <$> loadedGraph

  let graph = accumE Graph.empty $ unions [del, ins, lab, const <$> loadedGraph]
        where
          del = delElem <$> selected <@ action!Delete
          ins = createNode `union` createEdge
          lab = labelSimple <$> label <*> selected <@ action!Rename
          createNode = insNode <$> ((,) <$> newNode <*> label)
                       <@ action!CreateNode
          createEdge = makeEdge <$> selected2 <*> selected <*> label
                       <@ action!CreateEdge
          newNode = accumB 1 $
            (const <$> newNodeStart) `union` (pure (+1) <@ createNode)
  let graphB = stepper Graph.empty graph
  let graphS = showGraph <$> graphB
  reactimate $ save <$> graphS <*> file <@ action!Save
  reactimate $ putStrLn <$> graphS <@ action!Show
  reactimate $ loadWv wv <$> graph
  let clicks = unions $
                 (action !) <$> [CreateNode, CreateEdge, Rename]
  reactimate $ pure (entrySetText e1 "") <@ clicks
  let sel1lab = getLabel <$> graphB <*> selected
  let sel2lab = getLabel <$> graphB <*> selected2
  sink tb1 [textBufferText :== maybe "" show <$> sel1lab]
  sink tb2 [textBufferText :== maybe "" show <$> sel2lab]
  where
    loadWv webview x = do
      css <- readFile =<< getDataFileName "main.css"
      js <- readFile =<< getDataFileName "main.js"
      svg <- return . transformSvg css js =<< graphToSvg x
      webViewLoadString webview svg (Just "image/svg+xml") ""
    save t = \case "" -> actionActivate (actions!SaveAs); f -> writeFile f t
    maybeRead = \case "" -> Nothing; f -> Just $ readFile f

setUpGUI :: Builder -> IO Widgets
setUpGUI = flip fromBuilder $ do
  sw <- "scrolledwindow1"
  wi <- "window1"
  vb <- "vbox1"
  wv <- liftIO webViewNew
  od <- liftIO $ openDialog (wi::Window)
  liftIO $ set (sw::ScrolledWindow) [ containerChild := wv ]
  actions <- liftIO $ setupMenuToolBars wi (vb::VBox) $ fromList
    [ (About, aboutDialog)
    , (Open, runDialog od)
    ]
  Widgets <$> "entry1" <*> "textbuffer1" <*> "textbuffer2"
          <*> return wv <*> return od <*> return actions

aboutDialog :: IO ()
aboutDialog = do
  dialog <- aboutDialogNew
  windowSetPosition dialog WinPosCenter
  set dialog [aboutDialogVersion := versionText]
  dialogRun dialog >> widgetDestroy dialog

openDialog :: Window -> IO FileChooserDialog
openDialog parent = do
  dialog <- fileChooserDialogNew Nothing (Just parent) FileChooserActionOpen
              [(stockCancel,ResponseCancel),(stockOk,ResponseAccept)]
  windowSetPosition dialog WinPosCenterOnParent
  return dialog

runDialog :: DialogClass self => self -> IO ()
runDialog dialog = dialogRun dialog >> widgetHide dialog

