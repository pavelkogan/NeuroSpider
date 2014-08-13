{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NeuroSpider.Graph
  ( parseGraphEvent
  , showGraph
  , readGraph
  , gElem
  , getLabel
  , labelSimple
  , makeEdge
  , delElem
  ) where

import BasicPrelude hiding (try)
import Data.Default
import Data.Graph.Inductive.Graph
import Data.Map (fromList, toList, adjust)
import Data.Text (pack)
import Text.Parsec
import Text.Parsec.Text ()

newtype Graph' a b = Graph' { unGraph :: ([LNode a], [LEdge b]) }
  deriving (Show, Read)

showGraph :: (Graph gr, Show a, Show b) => gr a b -> Text
showGraph = show . Graph' . (labNodes &&& labEdges)

readGraph :: (Graph gr, Read a, Read b) => Text -> gr a b
readGraph = uncurry mkGraph . unGraph . read

type GraphElement = Either Edge Node
instance Default GraphElement where def = Left def

data GraphEvent = NodeClick Node | EdgeClick Edge
  deriving (Eq, Show)

gElem :: GraphEvent -> GraphElement
gElem (NodeClick n) = Right n
gElem (EdgeClick e) = Left e

parseGraphEvent :: Text -> Either ParseError GraphEvent
parseGraphEvent s = parse p (textToString s) s where
  p = string "click:" >> choice'
        [ NodeClick <$> node <* eof
        , EdgeClick <$> edge ]
  choice' = choice . map try
  node = read . pack <$> many1 digit
  edge = (,) <$> node <* string "->" <*> node

getLabel :: Graph gr => gr a a -> GraphElement -> Maybe a
getLabel g = either (elab g) (lab g)

elab :: Graph gr => gr a b -> Edge -> Maybe b
elab g (n, m) = if gelem n g then lookup m $ lsuc g n else Nothing

makeEdge :: DynGraph gr
         => GraphElement -> GraphElement -> b -> gr a b -> gr a b
makeEdge (Right n) (Right m) l = insEdge (n,m,l)
makeEdge _ _ _ = id

delElem :: DynGraph gr => GraphElement -> gr a b -> gr a b
delElem = either delEdge delNode

labelNode :: DynGraph gr => a -> Node -> gr a b -> gr a b
labelNode l n g = case match n g of
  (Nothing, g')            -> g'
  (Just (i, n', _, o), g') -> (i, n', l, o) & g'

labelEdge :: DynGraph gr => b -> Edge -> gr a b -> gr a b
labelEdge l (n1,n2) g = case match n1 g of
  (Nothing, g')             -> g'
  (Just (i, n', nl, o), g') ->
    let newOut = adjust (const l) n2 `asMap` o
    in  (i, n', nl, newOut) & g'
  where asMap f = map swap . toList . f . fromList . map swap

labelSimple :: DynGraph gr
            => l -> GraphElement -> gr l l -> gr l l
labelSimple l = either (labelEdge l) (labelNode l)

