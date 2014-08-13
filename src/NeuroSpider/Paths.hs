module NeuroSpider.Paths where

import BasicPrelude
import qualified Paths_NeuroSpider as P

getDataFileName :: String -> IO FilePath
getDataFileName = fmap fromString . P.getDataFileName

