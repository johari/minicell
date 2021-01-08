{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Piet.DSL.Shake.Audio where

import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad
import Data.Binary
import qualified Data.ByteString as B
import Data.ByteString.Base64
import Data.ByteString.Char8
import Data.Char
import Data.Hashable
import Data.Maybe
import Data.String
import Data.Typeable
import Development.Shake
import Development.Shake.Database
import Development.Shake.FilePath
import GHC.Generics
import Piet.ShakeUtils
import Spreadsheet.Types
import System.FilePath.Glob

import qualified Data.Map

newtype AudioOverlay = AudioOverlay (B.ByteString, B.ByteString) deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult AudioOverlay = (B.ByteString, String)

newtype AudioLoop = AudioLoop (B.ByteString, Int) deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult AudioLoop = (B.ByteString, String)

shakeyAudioRules = do

  shakeyAudioOverlay <- addOracleCache $ \(AudioOverlay (audio0, audio1)) -> do
    withTempDir $ \myDir -> do
      let audio0Path = myDir </> "audio0.mp3"
      let audio1Path = myDir </> "audio1.mp3"
      let outputPath = myDir </> "result.mp3"

      liftIO $ B.writeFile audio0Path audio0
      liftIO $ B.writeFile audio1Path audio1

      cmd_
        "ffmpeg"
        [ "-i", audio0Path
        , "-i", audio1Path
        , "-filter_complex", "amerge"
        , "-ac", "2"
        , "-c:a", "libmp3lame"
        , "-q:a", "4"
        , outputPath
        ]

      myMp3 <- liftIO $ B.readFile outputPath
      pure $ (myMp3, "TODO: reserved for build log")

  shakeyAudioLoop <- addOracleCache $ \(AudioLoop (audio0, num0)) -> do
    withTempDir $ \myDir -> do
      let audio0Path = myDir </> "audio0.mp3"
      let outputPath = myDir </> "result.mp3"

      liftIO $ B.writeFile audio0Path audio0

      cmd_
        "ffmpeg"
        [ "-i", audio0Path
        , "-stream_loop", (show (num-1))
        , "-c:a", "libmp3lame"
        , outputPath
        ]

      myMp3 <- liftIO $ B.readFile outputPath
      pure $ (myMp3, "TODO: reserved for build log")

  return ()

shakeyAudioOverlay :: Spreadsheet -> B.ByteString -> B.ByteString -> IO String
shakeyAudioOverlay model audio0 audio1 = do
  let XShakeDatabase db = shakeDatabase model

  ([(myMp3, _)], after) <- shakeRunDatabase db [(askOracle (AudioOverlay (audio0, audio1)))]
  shakeRunAfter shakeOptions after

  blobId <- addBlobToStorage model myMp3
  return $ blobId

shakeyAudioLoop :: Spreadsheet -> B.ByteString -> Int -> IO String
shakeyAudioLoop model audio0 num0 = do
  let XShakeDatabase db = shakeDatabase model

  ([(myMp3, _)], after) <- shakeRunDatabase db [(askOracle (AudioLoop (audio0, num0)))]
  shakeRunAfter shakeOptions after

  blobId <- addBlobToStorage model myMp3
  return $ blobId
