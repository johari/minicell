{-# LANGUAGE OverloadedStrings #-}



module Minicell.Interop.PDF where


import System.Log.Logger (infoM)

import Minicell.Cache

import Data.Foldable
import Data.Text         (Text, pack, unpack)
import Data.Text.IO as T (putStrLn)
import Data.Monoid       ((<>))


import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)

import Data.Maybe
import Data.String

import Data.Binary
import System.Directory

import System.IO.Temp
import System.Process

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Data.Digest.Pure.MD5


crop imagePath (w, h, x0, y0) = do
    imageFullPath <- do
        root <- fullCacheRoot
        return $ root <> "/" <> imagePath

    fileContent <- LB.readFile imageFullPath
    let imageHash = show $ md5 fileContent
    let cacheKey = ("minicell-io-crop", mconcat [imageHash, "-", show w, "-", show h, "-", show x0, "-", show y0, ".png"])

    fileExist <- doesObjectExist cacheKey


    case fileExist of
        True -> do
            infoM "wiki.sheets.cache.hit" (show cacheKey)
            return ()
        _-> do
            infoM "wiki.sheets.cache.miss" (show cacheKey)
            withSystemTempDirectory "minicell-crop" $ \tmp -> do

                -- Convert PDF to a collection of PNGs

                readProcess "convert" [ imageFullPath
                                      , "-crop" , (mconcat [show w, "x", show h, "+", show x0, "+", show y0, "!"])
                                      , (mconcat [tmp, "foo.png"])
                                      ] ""

                -- Pick the appropriate PNG
                -- Save it in Minicell's cache


                content <- B.readFile (mconcat [tmp, "foo.png"])
                storeObject cacheKey content
                return ()
    return (pathForKey cacheKey)

magicPdf url page = do
    -- Retrieve the PDF from url
    (pdfHash, pdfPath) <- retrieveAndStoreUrlToFile url

    -- Cache key for the PNG of the desired `page`
    let cacheKey =  ("minicell-io-pdf-page", mconcat [pdfHash, "-", show page, ".png"])

    fileExist <- doesObjectExist cacheKey

    -- Return the address so that cells can render it
    case fileExist of
        True -> return ()
        _ -> withSystemTempDirectory "minicell-magic-pdf" $ \tmp -> do

                -- Convert PDF to a collection of PNGs

                readProcess "convert" [ "-density", "100"
                                      , "-resize" , "800"
                                      , pdfPath
                                      , (mconcat [tmp, "foo.png"])
                                      ] ""


                -- <rant timestamp="2:08AM">
                -- `convert` is not consistent with the way it converts
                -- 1-page pdfs to png, and many-page pdfs to png
                --
                -- I mean...
                -- ¯\_(ツ)_/¯
                --

                perhapsSinglePagePdf <- doesFileExist $ mconcat [tmp, "foo.png"]
                let finalFilename =
                        case perhapsSinglePagePdf of
                            True -> "foo.png"
                            False -> mconcat ["foo-", (show page), ".png"]

                -- </rant>

                -- Pick the appropriate PNG
                -- Save it in Minicell's cache
                content <- B.readFile (mconcat [tmp, finalFilename])
                storeObject cacheKey content
                return ()

    return (pathForKey cacheKey)
