{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Piet.DSL.Shake.Audio where

import Data.FileEmbed
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

newtype AudioConcat = AudioConcat (B.ByteString, B.ByteString) deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult AudioConcat = (B.ByteString, String)

newtype AudioCut = AudioCut (B.ByteString, Maybe String, Maybe String) deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult AudioCut = (B.ByteString, String)

newtype AudioWaveform = AudioWaveform (B.ByteString) deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult AudioWaveform = (B.ByteString, String)

shakeyAudioRules = do

  addOracleCache $ \(AudioWaveform audio0) -> do
    withTempDir $ \myDir -> do
      let audio0Path = myDir </> "audio0.mp3"
      let datPath = myDir </> "audio.dat"
      let datAudioOnlyPath = myDir </> "audio_only.dat"
      let gpiPath = myDir </> "audio.gpi"
      let outputPath = myDir </> "audio.png" -- written inside the audio.gpi file

      liftIO $ B.writeFile audio0Path audio0
      liftIO $ B.writeFile gpiPath $(embedFile "src/Piet/DSL/Shake/audio.gpi")

      quietly $ cmd_ "sox" [audio0Path, datPath]
      Stdout out <- quietly $ command [BinaryPipes] "tail" ["-n+3", datPath]
      liftIO $ B.writeFile datAudioOnlyPath out
      quietly $ command_ [Cwd myDir] "gnuplot" [gpiPath]


      myPng <- liftIO $ B.readFile outputPath
      pure $ (myPng, "TODO: reserved for build log")

  addOracleCache $ \(AudioOverlay (audio0, audio1)) -> do
    withTempDir $ \myDir -> do
      let audio0Path = myDir </> "audio0.mp3"
      let audio1Path = myDir </> "audio1.mp3"
      let outputPath = myDir </> "result.mp3"

      liftIO $ B.writeFile audio0Path audio0
      liftIO $ B.writeFile audio1Path audio1

      quietly $ cmd_
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

  addOracleCache $ \(AudioConcat (audio0, audio1)) -> do
    withTempDir $ \myDir -> do
      let audio0Path = myDir </> "audio0.mp3"
      let audio1Path = myDir </> "audio1.mp3"
      let outputPath = myDir </> "result.mp3"

      liftIO $ B.writeFile audio0Path audio0
      liftIO $ B.writeFile audio1Path audio1

      quietly $ cmd_
        "sox"
        [ audio0Path
        , audio1Path
        , outputPath
        ]

      myMp3 <- liftIO $ B.readFile outputPath
      pure $ (myMp3, "TODO: reserved for build log")

  addOracleCache $ \(AudioLoop (audio0, num0)) -> do
    withTempDir $ \myDir -> do
      let audio0Path = myDir </> "audio0.mp3"
      let outputPath = myDir </> "result.mp3"

      liftIO $ B.writeFile audio0Path audio0

      quietly $ cmd_
        "ffmpeg"
        [ "-stream_loop", (show (num0-1))
        , "-i", audio0Path
        , "-c:a", "libmp3lame"
        , outputPath
        ]

      myMp3 <- liftIO $ B.readFile outputPath
      pure $ (myMp3, "TODO: reserved for build log")

  addOracleCache $ \(AudioCut (audio0, startPos, endPos)) -> do
    withTempDir $ \myDir -> do
      let audio0Path = myDir </> "audio0.mp3"
      let outputPath = myDir </> "result.mp3"

      liftIO $ B.writeFile audio0Path audio0

      let positioningArgs = case (startPos, endPos) of
            (Just startPos', Just endPos') -> ["-ss", startPos', "-t", endPos']
            _ -> error "flexible start and end positions not implemented for audio"

      quietly $ cmd_
        "ffmpeg"
        ([ "-i", audio0Path] ++ positioningArgs ++ ["-acodec", "copy", outputPath])

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

shakeyAudioConcat :: Spreadsheet -> B.ByteString -> B.ByteString -> IO String
shakeyAudioConcat model audio0 audio1 = do
  let XShakeDatabase db = shakeDatabase model

  ([(myMp3, _)], after) <- shakeRunDatabase db [(askOracle (AudioConcat (audio0, audio1)))]
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

shakeyAudioWaveform :: Spreadsheet -> B.ByteString -> IO String
shakeyAudioWaveform model audio0 = do
  let XShakeDatabase db = shakeDatabase model

  ([(myPng, _)], after) <- shakeRunDatabase db [(askOracle (AudioWaveform audio0))]
  shakeRunAfter shakeOptions after

  blobId <- addBlobToStorage model myPng
  return $ blobId

shakeyAudioCut :: Spreadsheet -> B.ByteString -> Maybe String -> Maybe String -> IO String
shakeyAudioCut model audio0 startPos endPos = do
  let XShakeDatabase db = shakeDatabase model

  ([(myMp3, _)], after) <- shakeRunDatabase db [(askOracle (AudioCut (audio0, startPos, endPos)))]
  shakeRunAfter shakeOptions after

  blobId <- addBlobToStorage model myMp3
  return $ blobId
