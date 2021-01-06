module Piet.DSL.Graphics.PDF where

import Piet.DSL.Shake.PDF
import Spreadsheet.Types


eval' :: (Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula
eval' eval model expr = case normalizeOp expr of
  EApp "PDF" [urlE, pageNumberE] -> do
    ESLit url <- eval model urlE
    EILit pageNumber <- eval model pageNumberE

    putStrLn "fetching PDF"
    let XShakeDatabase db = shakeDatabase model
    blobIds <- shakeyFetchPdf model url
    print blobIds
    putStrLn "PDF fetched"

    -- return (EImage $ "data:text/plain;base64," ++ (encodedPages !! page))
    return (EBlobId (blobIds !! pageNumber))
    -- return (ESLit ":)")

  EApp "CROP" [imgE, wE, hE, x0E, y0E] -> do
    img <- eval model imgE
    EBlob blob <- eval model (EApp "BLOB" [img])

    EILit x0 <- eval model x0E
    EILit y0 <- eval model y0E

    EILit w <- eval model wE
    EILit h <- eval model hE

    let XShakeDatabase db = shakeDatabase model
    encodedImage <- shakeyCropImage db blob (w, h, x0, y0)
    return (EImage $ "data:text/plain;base64," ++ encodedImage)

  EApp "MONO" [imgE] -> do
    EImage sourceImagePath <- eval model imgE
    EBlob blob <- eval model (EApp "BLOB" [EImage sourceImagePath])

    let XShakeDatabase db = shakeDatabase model
    encodedImage <- shakeyMonochrome db blob
    return (EImage $ "data:text/plain;base64," ++ encodedImage)
  _ -> return $ ENotImplemented