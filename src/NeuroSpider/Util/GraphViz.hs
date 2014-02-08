module NeuroSpider.Util.GraphViz where

import Data.GraphViz
import Data.Text.Lazy (Text)
import System.IO.Strict (hGetContents)

dotToSvg :: Text -> IO String
dotToSvg t = graphvizWithHandle Dot d Svg hGetContents
  where d = parseDotGraph t :: DotGraph String

