{-# LANGUAGE RecordWildCards #-}

module TreeStore
( TreeStore
, newWithForest
, getRow
)
where

import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Tree
import qualified Data.Maybe as Maybe
import Data.IORef
import qualified Graphics.UI.Gtk as Gtk
import qualified System.Glib as Glib

newtype TreeStore a = TreeStore (Gtk.CustomStore (Private a) a)

instance Gtk.TreeModelClass (TreeStore a)
instance Gtk.TypedTreeModelClass TreeStore
instance Gtk.GObjectClass (TreeStore a) where
  toGObject (TreeStore store) = Gtk.toGObject store
  unsafeCastGObject = TreeStore . Glib.unsafeCastGObject

newWithForest :: Show a => Forest a -> IO (TreeStore a)
newWithForest forest = do
  private <- loadForest forest
  new private

new :: Private a -> IO (TreeStore a)
new private =
  Gtk.customStoreNew private TreeStore (iface private) Nothing Nothing

getRow :: TreeStore a -> Gtk.TreeIter -> IO a
getRow = Gtk.customStoreGetRow

iface :: Private a -> Gtk.TreeModelIface a
iface private = Gtk.TreeModelIface {..}
  where
  treeModelIfaceGetFlags = return []
  treeModelIfaceGetIter path = do
    go path (itemsTree private)
    where
    go [] _ = return Nothing
    go [i] forest = case forest Vector.!? i of
      Nothing -> return Nothing
      Just itemRef -> do
        item <- readIORef itemRef
        let iter = mkIter (iid item)
        return (Just iter)
    go (i:is) forest = case forest Vector.!? i of
      Nothing -> return Nothing
      Just itemRef -> do
        item <- readIORef itemRef
        go is (children item)
  treeModelIfaceGetPath (Gtk.TreeIter _ _ _ itemId) = do
    let itemRef = itemsList private Vector.! (fromIntegral itemId)
    go [] itemRef
    where
    go res itemRef = do
      item <- readIORef itemRef
      case parent item of
        Nothing ->
          case Vector.elemIndex itemRef (itemsTree private) of
            Nothing -> error "treeModelIfaceGetPath: no index in root"
            Just i -> return $ reverse (i:res)
        Just parentRef -> do
          parentItem <- readIORef parentRef
          case Vector.elemIndex itemRef (children parentItem) of
            Nothing -> error "treeModelIfaceGetPath: no index in parent"
            Just i -> go (i:res) parentRef
  treeModelIfaceGetRow (Gtk.TreeIter _ _ _ itemId) = do
    let itemRef = itemsList private Vector.! (fromIntegral itemId)
    item <- readIORef itemRef
    return (content item)
  treeModelIfaceIterNext (Gtk.TreeIter _ _ _ itemId) = do
    let itemRef = itemsList private Vector.! (fromIntegral itemId)
    item <- readIORef itemRef
    siblings <- case parent item of
      Nothing -> return (itemsTree private)
      Just parentRef -> children <$> readIORef parentRef
    i <- case Vector.elemIndex itemRef siblings of
      Nothing -> error "treeModelIfaceIterNext: no index in parent"
      Just i -> return i
    if Vector.length siblings > succ i
      then do
        nextItem <- readIORef (siblings Vector.! succ i)
        let iter = mkIter (iid nextItem)
        return (Just iter)
      else do
        return Nothing
  treeModelIfaceIterChildren maybe_iter = do
    children <- case maybe_iter of
      Nothing -> return (itemsTree private)
      Just (Gtk.TreeIter _ _ _ itemId) -> do
        let itemRef = itemsList private Vector.! (fromIntegral itemId)
        children <$> readIORef itemRef
    case children Vector.!? 0 of
      Nothing -> return Nothing
      Just itemRef -> do
        item <- readIORef itemRef
        let iter = mkIter (iid item)
        return (Just iter)
  treeModelIfaceIterHasChild (Gtk.TreeIter _ _ _ itemId) = do
    let itemRef = itemsList private Vector.! (fromIntegral itemId)
    item <- readIORef itemRef
    return $ not $ Vector.null (children item)
  treeModelIfaceIterNChildren maybe_iter = do
    children <- case maybe_iter of
      Nothing -> return (itemsTree private)
      Just (Gtk.TreeIter _ _ _ itemId) -> do
        let itemRef = itemsList private Vector.! (fromIntegral itemId)
        children <$> readIORef itemRef
    return (Vector.length children)
  treeModelIfaceIterNthChild maybe_iter n = do
    children <- case maybe_iter of
      Nothing -> return (itemsTree private)
      Just (Gtk.TreeIter _ _ _ itemId) -> do
        let itemRef = itemsList private Vector.! (fromIntegral itemId)
        children <$> readIORef itemRef
    case children Vector.!? n of
      Nothing -> return Nothing
      Just itemRef -> do
        item <- readIORef itemRef
        let iter = mkIter (iid item)
        return (Just iter)
  treeModelIfaceIterParent (Gtk.TreeIter _ _ _ itemId) = do
    let itemRef = itemsList private Vector.! (fromIntegral itemId)
    item <- readIORef itemRef
    case parent item of
      Nothing -> return Nothing
      Just parentRef -> do
        parentItem <- readIORef parentRef
        let iter = mkIter (iid parentItem)
        return (Just iter)
  treeModelIfaceRefNode _ = return ()
  treeModelIfaceUnrefNode _ = return ()
  mkIter itemId = Gtk.TreeIter 0 0 0 itemId

data Private a = Private
  { itemsList :: Children a
  , itemsTree :: Children a
  }

data Item a = Item
  { iid :: IId
  , content :: a
  , children :: Children a
  , parent :: Maybe (ItemRef a)
  }

instance Show a => Show (Item a) where
  show Item{..} = show (iid, content, Vector.length children, Maybe.isJust parent)

type ItemRef a = IORef (Item a)

type Children a = Vector (ItemRef a)

type IId = Word32

loadForest :: Show a => Forest a -> IO (Private a)
loadForest forest = do
  listRef <- newIORef ([], 0)
  let go res _ [] = return (reverse res)
      go res parent (a:as) = do
        item <- loadItem parent a
        go (item : res) parent as

      loadItem parent node = do
        ref <- newIORef undefined
        iid <- do
          (l, i) <- readIORef listRef
          writeIORef listRef (ref:l, succ i)
          return i
        childrenList <- go [] (Just ref) (subForest node)
        let item = Item{..}
            content = rootLabel node
            children = Vector.fromList childrenList
        writeIORef ref item
        return ref

  itemsTree <- Vector.fromList <$> go [] Nothing forest
  itemsList <- Vector.fromList . reverse . fst <$> readIORef listRef
  return Private{..}
