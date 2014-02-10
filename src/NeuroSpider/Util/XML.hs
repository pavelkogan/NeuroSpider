{-# LANGUAGE OverloadedStrings #-}

module NeuroSpider.Util.XML where

import Data.Maybe
import Data.Text (Text)
import Data.XML.Types (Event(..))
import Text.XML
import Text.XML.Unresolved
import Text.XML.Writer

svgStyle :: Text -> XML
svgStyle = elementA "{http://www.w3.org/2000/svg}style"
                    [("type", "text/css")]
                    . content

xmlEvents :: XML -> [Event]
xmlEvents = concatMap elementToEvents . mapMaybe nodeElement . render

nodeElement :: Node -> Maybe Element
nodeElement (NodeElement e) = Just e
nodeElement _               = Nothing

elementToEvents :: Element -> [Event]
elementToEvents = filter (not . docEvent) . toEvents . toDoc
  where
    toDoc e = toXMLDocument $ Document (Prologue def def def) e def
    docEvent EventBeginDocument = True
    docEvent EventEndDocument   = True
    docEvent _                  = False

