{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NeuroSpider.Util.XML (transformSvg) where

import Prelude hiding (lookup)
import Data.Map (lookup, insert)
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

transformSvg :: Document -> Text -> Text -> Document
transformSvg d@(Document{..}) css js = d{documentRoot = documentRoot'}
  where
    documentRoot' = goElem documentRoot
    goElem e@(Element{..}) = case elementName of
      Name{nameLocalName = "svg",..} -> e{elementNodes = script++style++nodes}
      Name{nameLocalName = "g"  ,..} ->
        let newAttrs = addClick elementAttributes
        in  e{elementNodes = nodes, elementAttributes = newAttrs}
      _                              -> e{elementNodes = nodes}
      where nodes  = fmap goNode elementNodes
            style  = svgNodes "style"  css
            script = svgNodes "script" js
    goNode (NodeElement e) = NodeElement $ goElem e
    goNode n               = n
    addClick attrs = case lookup "class" attrs of
      Just a  -> if a `elem` ["node", "edge"]
                   then insert "onclick" "clickHandler(this)" attrs
                   else attrs
      Nothing -> attrs
