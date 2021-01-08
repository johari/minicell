{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Piet.ShakeUtils where

import Data.ByteString.Char8
import Data.String
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad
import Data.Binary
import Data.Char
import Data.Hashable
import Data.Maybe
import Data.Typeable
import Development.Shake
import Development.Shake.Database
import Development.Shake.FilePath
import GHC.Generics
import Data.ByteString.Base64
import qualified Data.Map as M
import qualified Crypto.Hash.SHA1 as H
import qualified Data.ByteString.Char8 as C
import Text.Printf (printf)

import Network.URI

import Spreadsheet.Types

import Web.DataUrl


import Data.Map
import Data.String

newtype HttpGet = HttpGet String deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult HttpGet = EExpr


shakeyRules = do
  shakeHttpGet <- addOracleCache $ \(HttpGet url) -> do
    case parseDataUrl (fromString url) of
      Left _ -> do
        let encodedUri = escapeURIString isUnescapedInURI url
        case parseURI encodedUri of
          Just uri -> do
            let Just myAuthority = uriAuthority uri
            if (uriRegName myAuthority) `Prelude.elem` ["youtu.be", "youtube.com", "mobile.youtube.com", "www.youtube.com", "vimeo.com", "soundcloud.com"] then do
              withTempDir $ \myDir -> do
                let outputPath = myDir </> "output.%(ext)s"
                cmd_ "youtube-dl" ["-x", url, "-o", outputPath, "--audio-format=mp3"]
                blob <- liftIO $ C.readFile (outputPath -<.> "mp3")
                pure $ EBlob blob "application/octet-stream"
            else do
              Stdout out <- command [BinaryPipes] "curl" ["-L", url, "--output", "-"] -- Nima being super lazy. No error handling.
              pure $ EBlob out "application/octet-stream"
          _ -> do
            Stdout out <- command [BinaryPipes] "curl" ["-L", url, "--output", "-"] -- Nima being super lazy. No error handling.
            pure $ EBlob out "application/octet-stream"

      Right rawData -> pure $ EBlob (du_data rawData) "application/octet-stream"

  return ()


addBlobsToStorage model blobs = do
  let XBlobStorage blobStorageTVar = blobStorage model
  let updates = [(sha1FromByteString blob, blob) | blob <- blobs]
  atomically $ modifyTVar blobStorageTVar (\t -> Data.Map.union t (Data.Map.fromList updates))
  return (fst <$> updates)

addBlobToStorage model blob = do
  let XBlobStorage blobStorageTVar = blobStorage model
  let blobId = sha1FromByteString blob
  atomically $ modifyTVar blobStorageTVar (Data.Map.insert blobId blob)
  return blobId

toHex :: C.ByteString -> String
toHex bytes = C.unpack bytes >>= printf "%02x"

sha1FromByteString :: C.ByteString -> String
sha1FromByteString bs = (toHex $ H.hash bs)