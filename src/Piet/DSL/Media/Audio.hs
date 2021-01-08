module Piet.DSL.Media.Audio where

import Spreadsheet.Types
import Piet.DSL.Shake.Audio

eval' :: (Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula
eval' eval model expr = case normalizeOp expr of
  EApp "AUDIO" [ expr ] -> do
    ESLit src <- eval model expr

    EBlobId blob _ <- eval model (EApp "BLOB" [EAudio src])

    return $ EBlobId blob "audio/mpeg"

  EApp "AOLAY" [ expr0, expr1 ] -> do
    preBlob0 <- eval model expr0
    EBlob blob0 _ <- eval model (EApp "BLOB" [preBlob0])

    preBlob1 <- eval model expr1
    EBlob blob1 _ <- eval model (EApp "BLOB" [preBlob1])

    let XShakeDatabase db = shakeDatabase model
    blobId <- shakeyAudioOverlay model blob0 blob1

    return (EBlobId blobId "audio/mpeg")

  EApp "LOOP" [ audioE, numE ] -> do
    preBlob <- eval model audioE
    EILit num <- eval model numE

    EBlob blob _ <- eval model (EApp "BLOB" [preBlob])

    let XShakeDatabase db = shakeDatabase model
    blobId <- shakeyAudioLoop model blob num

    return (EBlobId blobId "audio/mpeg")

  _ -> return ENotImplemented
  -- EApp "ACONCAT" [ expr1, expr2 ] -> do
  --   EAudio src1 <- eval model expr1
  --   EAudio src2 <- eval model expr2
  --   audioPath <- do
  --                   h <- getHomeDirectory
  --                   return (h ++ "/Dropbox/minicell-uploads/audio/")

  --   let fullSrc1 = mconcat [ audioPath, src1 ]
  --       fullSrc2 = mconcat [ audioPath, src2 ]

  --   fileContent1 <- LB.readFile fullSrc1
  --   fileContent2 <- LB.readFile fullSrc2
  --   let cacheKey = mconcat ["ACONCAT", show $ md5 fileContent1, show $ md5 fileContent2 ]

  --   let targetPath = mconcat [ audioPath, cacheKey, ".mp3" ]

  --   exists <- doesFileExist targetPath

  --   if exists then return () else
  --     withSystemTempDirectory "minicell-audio" $ \tmp -> do
  --       print tmp
  --       readProcess "sox" [ fullSrc1
  --                         , fullSrc2
  --                         , "output.mp3"
  --                         ] ""

  --       copyFile "output.mp3" targetPath
  --       return ()

  --   return (EAudio $ targetPath)