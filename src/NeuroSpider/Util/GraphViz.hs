module NeuroSpider.Util.GraphViz (graphToSvg) where

import Control.Monad
import Data.Graph.Inductive.Graph
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.String
import Data.Text.Lazy (pack)
import System.IO.Strict (hGetContents)

dotToSvg :: IsString s => DotGraph Node -> IO s
dotToSvg d = graphvizWithHandle Dot d Svg get
  where get = return.fromString <=< hGetContents

graphToSvg :: (IsString s, Graph gr, Labellable nl, Labellable el)
           => gr nl el -> IO s
graphToSvg = dotToSvg . graphToDot params
  where params = nonClusteredParams {
    fmtNode = \(n,l)     -> [toLabel l, ID (pack $ show n)],
    fmtEdge = \(n, m, l) ->
      [toLabel l, ID (pack $ show n ++ "->" ++ show m)] }
