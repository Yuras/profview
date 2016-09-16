
module Entry
where

import Data.ByteString (ByteString)

data Entry = Entry
  { ccLabel :: ByteString
  , moduleName :: ByteString
  , location :: Maybe ByteString
  , ccId :: ByteString
  , nEntries :: ByteString
  , indTime :: ByteString
  , indAlloc :: ByteString
  , inhTime :: ByteString
  , inhAlloc :: ByteString
  }
  deriving (Show)
