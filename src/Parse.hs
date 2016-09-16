{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parse
( parse
, scanner
)
where

import Entry

import qualified Data.Char as Char
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Tree
import Control.Monad
import Scanner (Scanner)
import qualified Scanner

data Conf = Conf
  { hasSrcLoc :: Bool
  }

parse :: Lazy.ByteString -> Either String (Tree Entry)
parse = Scanner.scanLazy scanner

scanner :: Scanner (Tree Entry)
scanner = do
  conf <- skipToHeader
  fst <$> scanTree conf 0

scanTree
  :: Conf
  -> Int                        -- ^ identation level
  -> Scanner (Tree Entry, Int)  -- ^ return identation level of the next entry
scanTree conf rootIdent = do
  root <- scanEntry conf
  ident <- ByteString.length <$> Scanner.takeWhileChar8 Char.isSpace
  (forest, nextIdent) <- go ident []
  return (Node root forest, nextIdent)
  where
  go ident res = do
    if ident <= rootIdent
      then return (reverse res, ident)
      else do
        (e, ident') <- scanTree conf ident
        go ident' (e:res)

scanEntry :: Conf -> Scanner Entry
scanEntry _ = do
  ccLabel <- takeName
  Scanner.skipSpace
  moduleName <- takeName
  Scanner.skipSpace
  let location = Nothing
  ccId <- takeName
  Scanner.skipSpace
  nEntries <- takeName
  Scanner.skipSpace
  indTime <- takeName
  Scanner.skipSpace
  indAlloc <- takeName
  Scanner.skipSpace
  inhTime <- takeName
  Scanner.skipSpace
  inhAlloc <- takeName
  skipNL
  return Entry{..}

takeName :: Scanner ByteString
takeName = Scanner.takeWhileChar8 (not . Char.isSpace)

takeLine :: Scanner ByteString
takeLine = do
  l <- Scanner.takeWhileChar8 (not . isNL)
  skipNL
  return l

skipNL :: Scanner ()
skipNL = void $ Scanner.takeWhileChar8 isNL

isNL :: Char -> Bool
isNL '\n' = True
isNL '\r' = True
isNL _ = False

skipToHeader :: Scanner Conf
skipToHeader = do
  l <- takeLine
  case Scanner.scanOnly scanHeader l of
    Left _ -> skipToHeader
    Right conf -> return conf

scanHeader :: Scanner Conf
scanHeader = do
  Scanner.string "COST CENTRE"
  Scanner.skipSpace
  Scanner.string "MODULE"
  Scanner.skipSpace
  Scanner.string "no."
  Scanner.skipSpace
  Scanner.string "entries"
  Scanner.skipSpace
  Scanner.string "%time"
  Scanner.skipSpace
  Scanner.string "%alloc"
  Scanner.skipSpace
  Scanner.string "%time"
  Scanner.skipSpace
  Scanner.string "%alloc"
  Scanner.skipSpace
  return Conf
    { hasSrcLoc = False
    }
