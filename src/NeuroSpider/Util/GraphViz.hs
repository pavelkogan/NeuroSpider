module NeuroSpider.Util.GraphViz where

import Control.Monad
import Data.Graph.Inductive.Graph
import Data.GraphViz
import Data.String
import System.IO.Strict (hGetContents)

dotStringToSvg :: IsString s => String -> IO s
dotStringToSvg = dotToSvg . parseDotGraph . fromString

dotToSvg :: IsString s => DotGraph Node -> IO s
dotToSvg d = graphvizWithHandle Dot d Svg get
  where get = return.fromString <=< hGetContents

graphToSvg :: (IsString s, Graph gr, Labellable nl, Labellable el)
           => gr nl el -> IO s
graphToSvg = dotToSvg . graphToDot params
  where params = nonClusteredParams
                   { fmtNode = \(_,l)     -> [toLabel l]
                   , fmtEdge = \(_, _, l) -> [toLabel l] }
