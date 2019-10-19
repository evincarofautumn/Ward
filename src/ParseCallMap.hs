module ParseCallMap (fromFile, fromSource) where

import Types
import Data.Bifunctor
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL

-- | Parse the given callmap graph file.  Assumes UTF8 encoding (will use
-- U+FFFD replacement character on UTF8 decoding errors).
fromFile :: FilePath -> IO (Either String CallMap)
fromFile path = first fixError . fromSource <$> BSL.readFile path
  where fixError err = path ++ ": " ++ err

-- | @fromSource path txt@ parses @txt@ and returns the resulting 'CallMap'.
-- (The @path@ is used in error messages)
fromSource :: BSL.ByteString -> Either String CallMap
fromSource = Data.Aeson.eitherDecode

