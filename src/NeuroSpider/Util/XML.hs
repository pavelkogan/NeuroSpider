{-# LANGUAGE OverloadedStrings #-}

module NeuroSpider.Util.XML where

import Control.Monad
import Data.Conduit
import Data.Maybe
import Data.Text (Text)
import Data.XML.Types (Event(..))
import Text.XML
import Text.XML.Stream.Parse
import Text.XML.Writer
import qualified Data.Conduit.List as CL
import qualified Text.XML.Writer as W

svgStyle :: Text -> XML
svgStyle = elementA "{http://www.w3.org/2000/svg}style"
                    [("type", "text/css")]
                    . W.content

xmlEvents :: XML -> IO [Event]
xmlEvents = liftM concat . mapM elementToEvents
            . mapMaybe nodeElement . render

nodeElement :: Node -> Maybe Element
nodeElement (NodeElement e) = Just e
nodeElement _               = Nothing

elementToEvents :: Element -> IO [Event]
elementToEvents e = stream $= filt $$ CL.consume
  where
    stream = renderBytes def doc $= parseBytes def
    doc    = Document (Prologue def def def) e def
    filt   = awaitForever stripDoc
    stripDoc EventBeginDocument = return ()
    stripDoc EventEndDocument   = return ()
    stripDoc event              = yield event

