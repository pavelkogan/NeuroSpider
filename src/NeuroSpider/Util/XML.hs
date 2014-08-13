{-# LANGUAGE RecordWildCards #-}

module NeuroSpider.Util.XML (transformSvg) where

import BasicPrelude hiding (lookup, insert)
import Data.Map (lookup, insert)
import Text.XML
import Text.XML.Writer
import qualified Data.Text.Lazy as Lazy (toStrict, fromStrict)

svgNodes :: String -> Text -> [Node]
svgNodes tag = elem' (name tag) attrs
  where
    name = fromString . ("{http://www.w3.org/2000/svg}"++)
    attrs = case tag of
      "style"  -> [("type","text/css")]
      "script" -> [("type", "text/ecmascript")]
      _        -> []

elem' :: Name -> [(Name, Text)] -> Text -> [Node]
elem' t as = render . elementA t as . content

transformSvg :: Text -> Text -> Text -> Text
transformSvg css js = renderText' . transformSvg_ css js . parseText'
  where
    renderText' = Lazy.toStrict . renderText def
    parseText' = parseText_ def . Lazy.fromStrict

transformSvg_ :: Text -> Text -> Document -> Document
transformSvg_ css js d@(Document{..}) = d{documentRoot = documentRoot'}
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

