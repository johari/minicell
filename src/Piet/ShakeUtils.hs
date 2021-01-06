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

import Spreadsheet.Types

newtype HttpGet = HttpGet String deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult HttpGet = EExpr

toHex :: C.ByteString -> String
toHex bytes = C.unpack bytes >>= printf "%02x"

sha1FromByteString :: C.ByteString -> String
sha1FromByteString bs = (toHex $ H.hash bs)