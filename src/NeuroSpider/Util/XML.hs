{-# LANGUAGE OverloadedStrings #-}

module NeuroSpider.Util.XML where

import Data.Text.Lazy
import Data.String
import Text.XML
import Text.XML.Writer
import qualified Data.Text as S

svgNodes :: String -> Text -> [Node]
svgNodes tag = elem' (name tag) attrs
  where
    name = fromString . ("{http://www.w3.org/2000/svg}"++)
    attrs = case tag of
      "style"  -> [("type","text/css")]
      "script" -> [("type", "text/ecmascript")]
      _        -> []

elem' :: Name -> [(Name, S.Text)] -> Text -> [Node]
elem' t as = render . elementA t as . content . toStrict

