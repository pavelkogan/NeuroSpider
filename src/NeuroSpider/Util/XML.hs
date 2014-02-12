{-# LANGUAGE OverloadedStrings #-}

module NeuroSpider.Util.XML where

import Data.Text.Lazy
import Text.XML
import Text.XML.Writer
import qualified Data.Text as S

svgStyle :: Text -> [Node]
svgStyle = elem' "{http://www.w3.org/2000/svg}style" [("type","text/css")]

elem' :: Name -> [(Name, S.Text)] -> Text -> [Node]
elem' t as = render . elementA t as . content . toStrict

