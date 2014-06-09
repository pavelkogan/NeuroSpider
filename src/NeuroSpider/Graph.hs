{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NeuroSpider.Graph where

import Control.Applicative
import Data.Default
import Data.Graph.Inductive.Graph
import Data.Map (fromList, toList, adjust)
import Data.Tuple (swap)
import Safe (readDef)
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

labelElem :: (DynGraph gr, Read a, Read b, Default a, Default b)
          => String -> GraphElement -> gr a b -> gr a b
labelElem l = either (labelEdge $ readDef def l)
                     (labelNode $ readDef def l)
