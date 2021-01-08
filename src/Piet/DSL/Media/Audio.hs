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

  EApp "ACONCAT" [ expr0, expr1 ] -> do
    preBlob0 <- eval model expr0
    EBlob blob0 _ <- eval model (EApp "BLOB" [preBlob0])

    preBlob1 <- eval model expr1
    EBlob blob1 _ <- eval model (EApp "BLOB" [preBlob1])

    let XShakeDatabase db = shakeDatabase model
    blobId <- shakeyAudioConcat model blob0 blob1

    return (EBlobId blobId "audio/mpeg")

  EApp "ALOOP" [ audioE, numE ] -> do
    preBlob <- eval model audioE
    EILit num <- eval model numE

    EBlob blob _ <- eval model (EApp "BLOB" [preBlob])

    let XShakeDatabase db = shakeDatabase model
    blobId <- shakeyAudioLoop model blob num

    return (EBlobId blobId "audio/mpeg")

  EApp "AWAVE" [ audioE ] -> do
    preBlob <- eval model audioE

    EBlob blob _ <- eval model (EApp "BLOB" [preBlob])

    let XShakeDatabase db = shakeDatabase model
    blobId <- shakeyAudioWaveform model blob

    return (EBlobId blobId "image/png")

  EApp "ACUT" [ audioE, startPosE, endPosE ] -> do
    preBlob <- eval model audioE
    ESLit startPos <- (eval model startPosE) >>= (\x -> return $ upcastToDurationString x)
    ESLit endPos <- (eval model endPosE) >>= (\x -> return $ upcastToDurationString x)

    EBlob blob _ <- eval model (EApp "BLOB" [preBlob])

    let XShakeDatabase db = shakeDatabase model
    blobId <- shakeyAudioCut model blob (Just startPos) (Just endPos)

    return (EBlobId blobId "audio/mpeg")

  EApp "ACROP" args -> do
    eval model (EApp "ACROP" args)

  _ -> return ENotImplemented
