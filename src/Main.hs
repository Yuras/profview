{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
( main
)
where

import qualified TreeStore
import qualified UI
import Parse (parse)

import qualified Data.ByteString.Lazy as Lazy.ByteString
import Control.Monad
import System.Environment (getArgs)
import qualified Graphics.UI.Gtk as Gtk

main :: IO ()
main = do
  tree <- do
    [path] <- getArgs
    bs <- Lazy.ByteString.readFile path
    case parse bs of
      Left err -> error err
      Right tree -> return tree

  void $ Gtk.initGUI
  window <- Gtk.windowNew
  Gtk.windowSetDefaultSize window 800 600
  void $ Gtk.on window Gtk.objectDestroy Gtk.mainQuit

  store <- TreeStore.newWithForest [tree]

  ui <- UI.create store
  Gtk.containerAdd window (UI.container ui)
  Gtk.treeViewExpandAll (UI.treeView ui)

  let UI.ToolItems{..} = UI.toolItems ui
  void $ Gtk.onToolButtonClicked expandAllItem $ do
    Gtk.treeViewExpandAll (UI.treeView ui)
  void $ Gtk.onToolButtonClicked collapseAllItem $ do
    Gtk.treeViewCollapseAll (UI.treeView ui)

  Gtk.widgetShowAll window
  Gtk.mainGUI
