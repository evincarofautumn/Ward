{-# language ScopedTypeVariables #-}
module DumpCallMap (encodeCallMap, hPutCallMap) where

import Types (CallMap(..))
import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString.Lazy as B
import System.IO (Handle, hFlush)
import qualified Data.Aeson as Aeson

-- | Encode the given callmap as a 'B.ByteString'
encodeCallMap :: CallMap -> B.ByteString
encodeCallMap = Aeson.encode

-- | @hPutCallMap handle callmap@ writes @callmap@ to the I/O handle @handle@.
-- The representation preserves all the source attributes of the underlying C file.
-- Identifiers are stored in UTF-8.
hPutCallMap :: MonadIO m => Handle -> CallMap -> m ()
hPutCallMap handle callMap = liftIO $ do
  B.hPut handle $ encodeCallMap callMap
  hFlush handle

