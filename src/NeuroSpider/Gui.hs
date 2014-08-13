module NeuroSpider.Gui (runGUI) where

import NeuroSpider.Graph
import NeuroSpider.UiManager
import NeuroSpider.Util.Gtk
import NeuroSpider.Util.GraphViz
import NeuroSpider.Util.XML
import NeuroSpider.Util.Reactive
import NeuroSpider.Paths
import Data.Graph.Inductive.Graph (nodeRange, insNode)
import qualified Data.Graph.Inductive.Graph as Graph (empty)
import qualified Data.Graph.Inductive.Tree as Graph

import BasicPrelude hiding (mapM, union)
import Data.Default
import Data.Map ((!), fromList, elems)
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
                 Entry
                 TextBuffer
                 TextBuffer
                 WebView
                 (Map Text Button)
                 (Map UiAction Action)

makeButtons :: IO (Map Text Button)
makeButtons = mapM (buttonNewWithLabel :: Text -> IO Button) $ fromList
  [ ("createNode", "Create Node")
  , ("createEdge", "Create Edge")
  , ("showGraph", "Show Graph")
  , ("saveGraph", "Save Graph")
  , ("loadGraph", "Load Graph")
  , ("deleteSelected", "Delete Selected")
  , ("renameSelected", "Rename Selected") ]

setUpGUI :: Builder -> IO Widgets
setUpGUI builder = do
  widgets <- fromBuilder builder $ Widgets
    $-> "entry1"
    *-> "entry2"
    *-> "textbuffer1"
    *-> "textbuffer2"
  sw <- "scrolledwindow1" builder :: IO ScrolledWindow
  bb <- "vbuttonbox1" builder :: IO VButtonBox
  vb <- "vbox1" builder :: IO VBox
  wi <- "window1" builder :: IO Window
  wv <- webViewNew
  set sw [ containerChild := wv ]
  buttons <- makeButtons
  mapM_ (containerAdd bb) $ elems buttons
  actions <- setupMenuToolBars wi vb
  return $ widgets wv buttons actions

runGUI :: IO ()
runGUI = doGUI $ withBuilder "main.glade" $ \builder -> do
  (graphAH, loadGraph) <- newAddHandler
  Widgets e1 e2 tb1 tb2 wv buttons actions <- setUpGUI builder
  run $ do
    nav <- eventN (\f _ request _ _ -> do
      uri <- networkRequestGetUri request
      case uri of
        Nothing -> return False
        Just u  -> f u >> return True
      ) wv navigationPolicyDecisionRequested
    button <- mapM (`event0` buttonActivated) buttons
    action <- mapM (`event0` actionActivated) actions
    fileString <- monitorAttr e1 editableChanged entryText
    labelString <- monitorAttr e2 editableChanged entryText
    let file = Filepath.fromText <$> stepper "" fileString
    let label = fromString <$> stepper def labelString
    let (_, click) = split $ parseGraphEvent <$> nav
    let select = gElem <$> click
    let selected = stepper def select
    let selected2 = stepper def $ selected <@ select

    let loadGraphE = filterJust $ maybeRead <$> file <@ button ! "loadGraph"
    reactimate $ (loadGraph =<<) <$> loadGraphE
    graphString <- fromAddHandler graphAH
    let loadedGraph = (readGraph :: Text -> Graph.Gr Text Text) <$> graphString
    let newNodeStart = (+1) . snd . nodeRange <$> loadedGraph

    let graph = accumE Graph.empty $ unions [del, ins, lab, const <$> loadedGraph]
          where
            del = delElem <$> selected <@ button ! "deleteSelected"
            ins = createNode `union` createEdge
            lab = labelSimple <$> label <*> selected <@ button ! "renameSelected"
            createNode = insNode <$> ((,) <$> newNode <*> label)
                         <@ button ! "createNode"
            createEdge = makeEdge <$> selected2 <*> selected <*> label
                         <@ button ! "createEdge"
            newNode = accumB 1 $
              (const <$> newNodeStart) `union` (pure (+1) <@ createNode)
    let graphB = stepper Graph.empty graph
    let graphS = showGraph <$> graphB
    reactimate $ maybeWrite <$> graphS <*> file <@ button ! "saveGraph"
    reactimate $ putStrLn <$> graphS <@ button ! "showGraph"
    reactimate $ loadWv wv <$> graph
    let clicks = unions $
                   (button !) <$> words "createNode createEdge renameSelected"
    reactimate $ pure (entrySetText e2 (""::Text)) <@ clicks
    let sel1lab = getLabel <$> graphB <*> selected
    let sel2lab = getLabel <$> graphB <*> selected2
    sink tb1 [textBufferText :== maybe "" show <$> sel1lab]
    sink tb2 [textBufferText :== maybe "" show <$> sel2lab]
  where
  loadWv wv x = do
    css <- readFile =<< getDataFileName "main.css"
    js <- readFile =<< getDataFileName "main.js"
    svg <- return . transformSvg css js =<< graphToSvg x
    webViewLoadString wv svg (Just "image/svg+xml") ""
  maybeWrite t = \case "" -> return (); f -> writeFile f t
  maybeRead = \case "" -> Nothing; f -> Just $ readFile f

