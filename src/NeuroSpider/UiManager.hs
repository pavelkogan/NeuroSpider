{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module NeuroSpider.UiManager
  ( setupMenuToolBars
  , UiAction(..)
  ) where

import BasicPrelude hiding (empty, on)
import Data.Char (isUpper)
import Data.Text (pack, unpack, toLower, breakOn)
import Graphics.UI.Gtk
import Text.XML
import Text.XML.Writer
import qualified Data.Map as Map (insert, empty, lookup)
import qualified Data.Text.Lazy as Lazy

data UiAction =
  File
  | New | Open | Save | SaveAs | Quit |
  Edit
  | Cut | Copy | Paste | Delete |
  Graph
  | CreateNode | CreateEdge | Rename | Show |
  Help
  | About
  deriving (Eq, Show, Read, Enum, Bounded, Ord)

name :: UiAction -> Text
name SaveAs = "Save _As"
name Cut = "Cu_t"
name CreateNode = "_Node"
name CreateEdge = "_Edge"
name a = ("_"<>) . unwords . upperSplit . show $ a

stock :: UiAction -> Text
stock CreateNode = stock_ "Add"
stock CreateEdge = stock_ "Connect"
stock Rename = stock_ "Convert"
stock Show = stock_ "Print"
stock a = stock_ $ show a

stock_ :: Text -> Text
stock_ = ("gtk-"<>) . toLower . intercalate "-" . upperSplit

data ActionActivate = Skip | FireEvent | DoIO (IO ())

activate :: UiAction -> ActionActivate
activate Quit                          = DoIO mainQuit
activate a | a `elem`
             [File,Edit,Graph,Help]    = Skip
           | otherwise                 = FireEvent

uiXmlString :: Text
uiXmlString = snd . breakOn "<ui>" . Lazy.toStrict . renderText def $ uiXml

uiXml :: Document
uiXml = ui $ do
  menubar $ do
    menu File $ do
      menuitem New
      menuitem Open
      menuitem Save
      menuitem SaveAs
      separator
      menuitem Quit
    menu Edit $ do
      menuitem Cut
      menuitem Copy
      menuitem Paste
      menuitem Delete
    menu Graph $ do
      menuitem CreateNode
      menuitem CreateEdge
      menuitem Rename
      menuitem Show
    menu Help $ do
      menuitem About
  toolbar $ do
    toolitem New
    toolitem Open
    toolitem Save
    separator
    toolitem Cut
    toolitem Copy
    toolitem Paste
    separator
    toolitem CreateNode
    toolitem CreateEdge
    toolitem Rename
    toolitem Delete
    separator
    toolitem Show
  where
    ui = document "ui"
    menubar = element "menubar"
    toolbar = element "toolbar"
    menu = elemA "menu"
    menuitem a = elemA "menuitem" a empty
    toolitem a = elemA "toolitem" a empty
    separator = element "separator" empty
    elemA e a = elementA e [("action", show a), ("name", name a)]

setupMenuToolBars :: (WindowClass w, BoxClass b)
         => w -> b -> Map UiAction (IO ())
         -> IO (Map UiAction Action)
setupMenuToolBars window box activations = do

  stockIds <- stockListIds
  actiongroup <- actionGroupNew "actiongroup"
  actions <- forM [minBound..maxBound] $ \a -> do
    let stock' = if stock a `elem` stockIds then Just (stock a) else Nothing
    action <- actionNew (show a) (name a) Nothing stock'
    actionGroupAddActionWithAccel actiongroup action (Nothing :: Maybe Text)
    let activation = maybe (activate a) DoIO $ Map.lookup a activations
    case activation of
      Skip      -> return id
      FireEvent -> return $ Map.insert a action
      DoIO io   -> on action actionActivated io >> return id

  uimanager <- uiManagerNew
  uiManagerInsertActionGroup uimanager actiongroup 0
  _ <- uiManagerAddUiFromString uimanager uiXmlString
  accelgroup <- uiManagerGetAccelGroup uimanager
  windowAddAccelGroup window accelgroup

  menubar' <- uiManagerGetWidget uimanager "ui/menubar"
  toolbar' <- uiManagerGetWidget uimanager "ui/toolbar"
  let menubar = maybe (error "menubar setup") castToMenuBar menubar'
  let toolbar = maybe (error "toolbar setup") castToToolbar toolbar'
  boxPackStart box menubar PackNatural 0
  boxPackStart box toolbar PackNatural 1

  return $ foldr ($) Map.empty actions

upperSplit :: Text -> [Text]
upperSplit = map pack . takeWhile (/= "") . dropWhile (== "")
           . map fst . iterate (upperSplit_ . snd) . ("",) . unpack

upperSplit_ :: String -> (String, String)
upperSplit_ s = if length startUpper > 1
                 then (startUpper, rest)
                 else (startUpper ++ s1, s2)
  where (startUpper, rest) = span isUpper s
        (s1, s2)           = break isUpper rest

