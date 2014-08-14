module NeuroSpider.Paths where

import BasicPrelude
import Data.Version
import qualified Paths_NeuroSpider as P

version :: Version
version = P.version

versionText :: Text
versionText = fromString $ showVersion version

getDataFileName :: String -> IO FilePath
getDataFileName = fmap fromString . P.getDataFileName

