module NeuroSpider.Util.GraphViz where

import Control.Monad
import Data.GraphViz
import Data.String
import System.IO.Strict (hGetContents)

dotToSvg :: IsString s => String -> IO s
dotToSvg t = graphvizWithHandle Dot d Svg get
  where
    d   = parseDotGraph $ fromString t :: DotGraph String
    get = return.fromString <=< hGetContents

