{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Piet.DSL.Shake.PDF where

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

import Web.DataUrl

newtype PdfToPngs = PdfToPngs B.ByteString deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult PdfToPngs = ([B.ByteString], String)

newtype UrlToPngs = UrlToPngs String deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult UrlToPngs = [B.ByteString]

newtype CropImage = CropImage (B.ByteString, (Int, Int, Int, Int)) deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult CropImage = (B.ByteString, String)

newtype Monochrome = Monochrome B.ByteString deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult Monochrome = (B.ByteString, String)

shakeyPdfRules = do
  want ["my_phony"]

  shakeHttpGet <- addOracleCache $ \(HttpGet url) -> do
    case parseDataUrl (fromString url) of
      Left _ -> do
        Stdout out <- command [BinaryPipes] "curl" ["-L", url, "--output", "-"] -- Nima being super lazy. No error handling.
        pure $ EBlob out
      Right rawData -> pure $ EBlob (du_data rawData)

  cachedPdfToPngs <- newCache $ \pdf -> do
    withTempDir $ \myDir -> do
      let pdfPath = myDir </> "untitled.pdf"
      let pngPath = myDir </> "foo.png"
      liftIO $ B.writeFile pdfPath pdf

      cmd_
        "convert"
        [ "-density",
          "100",
          "-resize",
          "800",
          pdfPath,
          pngPath
        ]

      -- Single page pdf or multiple page?
      singlePage <- doesFileExist pngPath
      case singlePage of
        True -> do
          png0 <- liftIO $ B.readFile pngPath
          pure $ ([png0], "TODO: reserved for build log")
        False -> do
          foos <- liftIO $ glob (replaceBaseName pngPath ((takeBaseName pngPath) ++ "-*"))
          pngs <- sequence ((\path -> liftIO $ B.readFile path) <$> foos)
          pure $ (pngs, "TODO: reserved for build log")

  shakeCropImage <- addOracleCache $ \(CropImage (originalImage, (w, h, x0, y0))) -> do
    withTempDir $ \myDir -> do
      let originalImagePath = myDir </> "orig.png" -- png?
      let croppedImagePath = myDir </> "cropped.png"

      liftIO $ B.writeFile originalImagePath originalImage

      cmd_
        "convert"
        [ originalImagePath,
          "-crop",
          (mconcat [show w, "x", show h, "+", show x0, "+", show y0, "!"]),
          croppedImagePath
        ]

      png0 <- liftIO $ B.readFile croppedImagePath
      pure $ (png0, "TODO: reserved for build log")

  shakeMonochrome <- addOracleCache $ \(Monochrome originalImage) -> do
    withTempDir $ \myDir -> do
      let originalImagePath = myDir </> "orig.png" -- png?
      let monoImagePath = myDir </> "mono.png"
      liftIO $ B.writeFile originalImagePath originalImage
      cmd_
        "convert"
        [ originalImagePath,
          "-monochrome",
          monoImagePath
        ]
      png0 <- liftIO $ B.readFile monoImagePath
      pure $ (png0, "TODO: reserved for build log")

  shakeUrlToPngs <- addOracleCache $ \(UrlToPngs url) -> do
    withTempDir $ \myDir -> do
      EBlob pdf <- shakeHttpGet (HttpGet url)
      (pngs, _) <- cachedPdfToPngs pdf
      pure pngs

  phony "my_phony" $ do
    pure ()

-- fetchPdf :: String -> IO [String]
-- fetchPdf url = do
--   let opts = shakeOptions
--   ([phony_result], after) <- shakeWithDatabase opts (rules "http://bangbangcon.com/west/images/logo.png") $ \db -> do
--       -- shakeOneShotDatabase db
--       shakeRunDatabase db [(askOracle (UrlToPngs "http://bangbangcon.com/west/images/logo.png"))]
--   -- shakeRunAfter shakeOptions after
--   return phony_result
-- -- fetchPdf url = do
-- --   shake shakeOptions rules

shakeyFetchPdf :: Spreadsheet -> String -> IO [String]
shakeyFetchPdf model url = do
  let XShakeDatabase db = shakeDatabase model
  ([buildResult], after) <- shakeRunDatabase db [(askOracle (UrlToPngs url))]
  shakeRunAfter shakeOptions after

  let res = [(sha1FromByteString blob, blob) | blob <- buildResult]
  let XBlobStorage blobStorageTVar = blobStorage model
  atomically $ modifyTVar blobStorageTVar (\t -> Data.Map.union t (Data.Map.fromList res))

  return (fst <$> res)

shakeyCropImage :: ShakeDatabase -> B.ByteString -> (Int, Int, Int, Int) -> IO String
shakeyCropImage db img coords = do
  ([buildResult], after) <- shakeRunDatabase db [(askOracle (CropImage (img, coords)))]
  shakeRunAfter shakeOptions after

  let (croppedImage, _) = buildResult
  return $ unpack (Data.ByteString.Base64.encode croppedImage)

shakeyMonochrome :: ShakeDatabase -> B.ByteString -> IO String
shakeyMonochrome db img = do
  ([buildResult], after) <- shakeRunDatabase db [(askOracle (Monochrome img))]
  shakeRunAfter shakeOptions after

  let (monoImage, _) = buildResult
  return $ unpack (Data.ByteString.Base64.encode monoImage)