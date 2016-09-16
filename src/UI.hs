{-# LANGUAGE RecordWildCards #-}

module UI
( UI(..)
, ToolItems(..)
, create
)
where

import Entry (Entry)
import qualified Entry
import TreeStore (TreeStore)
import qualified TreeStore

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Control.Monad
import qualified Graphics.UI.Gtk as Gtk

data UI = UI
  { treeView :: Gtk.TreeView
  , toolItems :: ToolItems
  , container :: Gtk.Container
  }

data ToolItems = ToolItems
  { expandAllItem :: Gtk.ToolButton
  , collapseAllItem :: Gtk.ToolButton
  }

create :: TreeStore Entry -> IO UI
create store = do
  treeView <- createTreeView store

  scrolled <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.containerAdd scrolled treeView

  vbox <- Gtk.vBoxNew False 0

  (toolbar, toolItems) <- createToolbar
  Gtk.boxPackStart vbox toolbar Gtk.PackNatural 0

  Gtk.boxPackStart vbox scrolled Gtk.PackGrow 0

  let container = Gtk.toContainer vbox
  return UI{..}

createToolbar :: IO (Gtk.Toolbar, ToolItems)
createToolbar = do
  toolbar <- Gtk.toolbarNew

  expandAllItem <- Gtk.toolButtonNew (Nothing :: Maybe Gtk.Widget)
    (Just "expand all")
  Gtk.toolbarInsert toolbar expandAllItem 0

  collapseAllItem <- Gtk.toolButtonNew (Nothing :: Maybe Gtk.Widget)
    (Just "collapse all")
  Gtk.toolbarInsert toolbar collapseAllItem 0

  return (toolbar, ToolItems{..})

createTreeView :: TreeStore Entry -> IO Gtk.TreeView
createTreeView store = do
  treeView <- Gtk.treeViewNewWithModel store

  Gtk.treeViewSetEnableSearch treeView True
  Gtk.treeViewSetSearchEqualFunc treeView $ Just $ \str iter -> do
    entry <- TreeStore.getRow store iter
    return $ Text.count str (Text.decodeUtf8 $ Entry.ccLabel entry) /= 0

  do
    column <- Gtk.treeViewColumnNew
    Gtk.treeViewColumnSetTitle column ("COST CENTRE" :: String)
    cell <- Gtk.cellRendererTextNew
    Gtk.cellLayoutPackStart column cell True
    Gtk.cellLayoutSetAttributes column cell store $ \entry ->
      [Gtk.cellText Gtk.:= Text.decodeUtf8 (Entry.ccLabel entry)]
    void $ Gtk.treeViewAppendColumn treeView column

  do
    column <- Gtk.treeViewColumnNew
    Gtk.treeViewColumnSetTitle column ("MODULE" :: String)
    cell <- Gtk.cellRendererTextNew
    Gtk.cellLayoutPackStart column cell True
    Gtk.cellLayoutSetAttributes column cell store $ \entry ->
      [Gtk.cellText Gtk.:= Text.decodeUtf8 (Entry.moduleName entry)]
    void $ Gtk.treeViewAppendColumn treeView column

  do
    column <- Gtk.treeViewColumnNew
    Gtk.treeViewColumnSetTitle column ("entries" :: String)
    cell <- Gtk.cellRendererTextNew
    Gtk.cellLayoutPackStart column cell True
    Gtk.cellLayoutSetAttributes column cell store $ \entry ->
      [Gtk.cellText Gtk.:= Text.decodeUtf8 (Entry.nEntries entry)]
    void $ Gtk.treeViewAppendColumn treeView column

  do
    column <- Gtk.treeViewColumnNew
    Gtk.treeViewColumnSetTitle column ("individual time" :: String)
    cell <- Gtk.cellRendererTextNew
    Gtk.cellLayoutPackStart column cell True
    Gtk.cellLayoutSetAttributes column cell store $ \entry ->
      [Gtk.cellText Gtk.:= Text.decodeUtf8 (Entry.indTime entry)]
    void $ Gtk.treeViewAppendColumn treeView column

  do
    column <- Gtk.treeViewColumnNew
    Gtk.treeViewColumnSetTitle column ("individual alloc" :: String)
    cell <- Gtk.cellRendererTextNew
    Gtk.cellLayoutPackStart column cell True
    Gtk.cellLayoutSetAttributes column cell store $ \entry ->
      [Gtk.cellText Gtk.:= Text.decodeUtf8 (Entry.indAlloc entry)]
    void $ Gtk.treeViewAppendColumn treeView column

  do
    column <- Gtk.treeViewColumnNew
    Gtk.treeViewColumnSetTitle column ("inherited time" :: String)
    cell <- Gtk.cellRendererTextNew
    Gtk.cellLayoutPackStart column cell True
    Gtk.cellLayoutSetAttributes column cell store $ \entry ->
      [Gtk.cellText Gtk.:= Text.decodeUtf8 (Entry.inhTime entry)]
    void $ Gtk.treeViewAppendColumn treeView column

  do
    column <- Gtk.treeViewColumnNew
    Gtk.treeViewColumnSetTitle column ("inherited alloc" :: String)
    cell <- Gtk.cellRendererTextNew
    Gtk.cellLayoutPackStart column cell True
    Gtk.cellLayoutSetAttributes column cell store $ \entry ->
      [Gtk.cellText Gtk.:= Text.decodeUtf8 (Entry.inhAlloc entry)]
    void $ Gtk.treeViewAppendColumn treeView column

  return treeView
