module NeuroSpider.GraphViz (graphToSvg) where

import BasicPrelude
import Data.Graph.Inductive.Graph
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import qualified Data.ByteString as Byte (hGetContents)
import qualified Data.Text.Lazy as Lazy (fromStrict)

dotToSvg :: DotGraph Node -> IO Text
dotToSvg d = decodeUtf8 <$> graphvizWithHandle Dot d Svg Byte.hGetContents

graphToSvg :: (Graph gr, Labellable nl, Labellable el)
           => gr nl el -> IO Text
graphToSvg = dotToSvg . graphToDot params
  where params = nonClusteredParams {
    fmtNode = \(n,l)     -> [toLabel l, ID (Lazy.fromStrict $ show n)],
    fmtEdge = \(n, m, l) ->
      [toLabel l, ID (Lazy.fromStrict $ show n ++ "->" ++ show m)] }

