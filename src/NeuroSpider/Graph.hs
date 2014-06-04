{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NeuroSpider.Graph where

import Control.Applicative
import Data.Default
import Data.Graph.Inductive.Graph
import Text.Parsec

type GraphElement = Either Edge Node
instance Default GraphElement where def = Left def

data GraphEvent = NodeClick Node | EdgeClick Edge
  deriving (Eq, Show)

gElem :: GraphEvent -> GraphElement
gElem (NodeClick n) = Right n
gElem (EdgeClick e) = Left e

parseGraphEvent :: String -> Either ParseError GraphEvent
parseGraphEvent s = parse p s s where
  p = string "click:" >> choice'
        [ node <* eof >>= return . NodeClick
        , edge >>= return . EdgeClick ]
  choice' = choice . map try
  node = many1 digit >>= return . read
  edge = do
    n <- node
    _ <- string "->"
    m <- node
    return (n, m)

elab :: Graph gr => gr a b -> Edge -> Maybe b
elab g (n, m) = lookup m $ lsuc g n

makeEdge :: DynGraph gr
         => GraphElement -> GraphElement -> b -> gr a b -> gr a b
makeEdge (Right n) (Right m) l = insEdge (n,m,l)
makeEdge _ _ _ = id

delElem :: DynGraph gr => GraphElement -> gr a b -> gr a b
delElem = either delEdge delNode
