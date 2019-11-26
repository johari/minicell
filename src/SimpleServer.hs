{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module SimpleServer where

import qualified Codec.Serialise as SSS

-- Logger stuff

import System.Log.Logger (Priority (DEBUG), debugM, infoM, setLevel,
                          updateGlobalLogger, warningM, noticeM,
                          rootLoggerName,
                          setHandlers, addHandler
                          )
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

-- Diagrams stuff

import Diagrams.Prelude hiding (value, (.=), (<>))
-- import Diagrams.Backend.Rasterific.Text
-- import Diagrams.Backend.Rasterific
import Diagrams.Backend.SVG
import Graphics.Svg.Core

-- Template stuff

import Text.Mustache


import Data.List (intercalate)
import Data.List.Extra (splitOn)

import Data.Aeson
import qualified Data.Text as T
import Control.Lens hiding ((.=), none)
import Data.List (find)
import Data.Monoid
import Data.String

import Data.Maybe

-- Time stuff

import Data.Time.Clock.POSIX

-- HTTP Client stuff
import Network.Wreq

-- HTTP Server stuff

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Network.Wai.Parse
import Network.Wai.Middleware.Cors
import Network.Mime

-- Bytestring stuff

import Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS


import Control.Concurrent.STM

import Control.Monad.IO.Class (liftIO)

-- Minicell stuff

import Minicell.Cache (fullCacheRoot)
import Spreadsheet.Types
import Spreadsheet as Mini

import Spreadsheet.Evaluator.Parser
import Text.ParserCombinators.Parsec

-- Graph stuff

import Data.Graph.Inductive.Dot
import Data.Graph.Inductive.Graph

import System.Process

import System.IO (stderr)

import qualified Data.Map
import qualified Data.List

import Data.FileEmbed

embeddedAssets :: [(FilePath, BS.ByteString)]
embeddedAssets = $(embedDir "../build")

-- embeddedAssets = [("main.html", $(embedFile "../static/main.html"))]

main2 = do
  h <- streamHandler stderr DEBUG >>= \lh -> return $
                  setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")

  updateGlobalLogger rootLoggerName (setHandlers [h])

  updateGlobalLogger "wiki.sheets.html" (setLevel DEBUG)

  s <- eval emptySpreadsheet (EApp "TPL" [ESLit "../examples/poppet/views/test.html"])
  print s

addFileLogger = do
    h <- fileHandler "/tmp/debug.log" DEBUG >>= \lh -> return $
             setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger rootLoggerName (addHandler h)

main = do
    let port = 3000
    modelTVar1 <- atomically $ newTVar emptySpreadsheet
    modelTVar2 <- atomically $ newTVar emptySpreadsheet

    let modelTVars = Data.Map.fromList [([], modelTVar1), (["42"], modelTVar2)]

    -- updateGlobalLogger rootLoggerName (setFormatter $ simpleLogFormatter "[$loggername] $msg")
    h <- streamHandler stderr DEBUG >>= \lh -> return $
                    setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")

    updateGlobalLogger rootLoggerName (setHandlers [h])

    -- updateGlobalLogger "wiki.sheets.http" (setLevel DEBUG)
    updateGlobalLogger "wiki.sheets.eval.eapp" (setLevel DEBUG)

    infoM "wiki.sheets.http" ("Listening on port " ++ show port)

    addFileLogger

    run port (app modelTVars)

app :: Data.Map.Map [String] (TVar Spreadsheet) -> Application
app modelTVars =
    websocketsOr  defaultConnectionOptions wsApp (simpleCors $ anyRoute modelTVars)

wsApp :: ServerApp
wsApp pending_conn = do
    conn <- acceptRequest pending_conn
    --t <- getCurrentTime
    let t = "hi"
    sendTextData conn $ (fromString ("Hello, client! " <> show t) :: T.Text)


eexprToHttpResponse cellValue = do
    case cellValue of
        ESLit s -> return $
            responseLBS status200
                        [(hContentType, "text/html")]
                        (fromString $ s)

        EImage src -> do
            -- Download the image
            -- Show the image
            print $ mconcat ["fetching ", src]
            -- response <- (get "http://poppet.us/favicon.ico")
            response <- (get src)
            return $ responseLBS status200 [] (response ^. responseBody)

        _ -> return $
            responseLBS status503
                        [(hContentType, "text/plain")]
                        (fromString $ "HTML output not implemented for " ++ (show cellValue))



eexprToHTML cellValue cometAddress  = do
    case cellValue of
        ESLit s -> return $ s
        _ -> return $ mconcat ["html not implemented for ", (show cellValue)]

eexprToComet cellValue cometAddress  = do
    -- This is the main point of integration betweenR
    -- A) Haskell values
    -- B) Elm values
    -- C) Transforming unusual values to something suitable to render in Frontend

    cachePath <- fullCacheRoot
    let basePath = [cachePath, "/", (addrToExcelStyle cometAddress)] -- don't forget the trailing slash!
    let dotPath = mconcat $ basePath <> [".dot"]
    let pngPath = mconcat $ basePath <> [".png"]
    let svgPath = mconcat $ basePath <> [".svg"]

    t <- getPOSIXTime
    let targetSrc ext = ("/minicell-cache/" ++ (addrToExcelStyle cometAddress) ++ "." ++ ext ++ "?" ++ (show t))

    case cellValue of
        EDiag (XDiagram myDiagram) -> do
            -- renderDia Rasterific (RasterificOptions (mkWidth 250)) myDiagram
            -- let myDiagram = (fc red . lw none $ circle 1) ||| (fc green . lw none $ circle 1) :: Diagram B
            let elem = renderDia SVG (SVGOptions (mkWidth 250) Nothing "" [] True) myDiagram
            renderToFile svgPath elem
            -- renderRasterific pngPath (mkWidth 250)

            return $ CometImage cometAddress (targetSrc "svg")

        EGraphFGL g -> do
            let dot = showDot (fglToDot g)

            writeFile dotPath dot
            system ("dot -Tpng -o" ++ pngPath ++ " " ++ dotPath)

            return $ CometImage cometAddress (targetSrc "png")

        ESLit s -> return $ CometSLit cometAddress s
        EHTML s -> return $ CometHTML cometAddress s
        EILit i -> return $ CometILit cometAddress i
        EImage src -> return $ CometImage cometAddress src
        EVideo src -> return $ CometVideo cometAddress src
        _ -> return $ CometSLit cometAddress (show cellValue)


endpointPrepare0 modelTVar cometKey = do
    let cometAddress = (cometKeyToAddr $ cometKey)

    model <- readTVarIO modelTVar


    -- FIXME
    -- This is a very wrong place to implement the cache
    -- There should be one layer between `eval` and `eexprToComet` that handles the cache

    cellValue <- eval model (ECellRef cometAddress)
    return (cellValue, cometAddress)

endpointPrepare modelTVar cometKey = do
    let cometAddress = (cometKeyToAddr $ cometKey)

    model <- readTVarIO modelTVar


    -- FIXME
    -- This is a very wrong place to implement the cache
    -- There should be one layer between `eval` and `eexprToComet` that handles the cache

    cellValue <- eval model (ECellRef cometAddress)



    val <- eexprToComet cellValue cometAddress
    return val

endpointPrepareHTML modelTVar cometKey = do
    let cometAddress = (cometKeyToAddr $ cometKey)

    model <- readTVarIO modelTVar


    -- FIXME
    -- This is a very wrong place to implement the cache
    -- There should be one layer between `eval` and `eexprToComet` that handles the cache

    cellValue <- eval model (ECellRef cometAddress)



    val <- eexprToHTML cellValue cometAddress
    return  val


endpointShow modelTVar cometKey req res = do
    val <- endpointPrepare modelTVar (T.unpack cometKey)

    res $ responseLBS status200
                          [(hContentType, "application/json")]
                          (encode val)

endpointShowHTML modelTVar cometKey req res = do
    val <- endpointPrepareHTML modelTVar (T.unpack cometKey)

    res $ responseLBS status200
                          [(hContentType, "text/html")]
                          (fromString val)

spillHelper cell =
    case value cell of
        EList vals -> [Cell { value = val, addr = (x0, y0+i)} | (i, val) <- zip [0..] vals, let (x0, y0) = addr cell ]
        EEmpty -> []
        _ -> [cell]


spill :: List Cell -> List Cell
spill el = concat $ (spillHelper <$> el)

endpointShowAll modelTVar req res = do
    model <- readTVarIO modelTVar

    let existingKeys = addrToExcelStyle <$> addr <$> (database model)

    preSpillModel <- sequence $ (endpointPrepare0 modelTVar) <$> existingKeys

    let newDatabase   = spill [ Cell { value = v, addr = a } | (v, a) <- preSpillModel ]
    let existingKeys' = addrToExcelStyle <$> addr <$> newDatabase
    modelTVar' <- atomically $ newTVar (model { database = newDatabase })


    allOfCells <- sequence $ ((endpointPrepare modelTVar') <$> existingKeys')

    res $ responseLBS status200
                          [(hContentType, "application/json")]
                          (encode allOfCells)

storeFiles :: [File B.ByteString] -> IO [String]
storeFiles [] = return []
storeFiles (x:xs) = do
    let (param, info) = x
    -- FIXME: param is actually important but we are ignoring it

    let uploadedFileName = C.unpack (fileName info)
        uploadedContent  = fileContent info

    B.writeFile ("../build/minicell-cache/" ++ (uploadedFileName)) uploadedContent

    otherUrls <- storeFiles xs

    return (["http://localhost:8000/minicell-cache/" ++ (uploadedFileName)] ++ otherUrls)

anyRoute modelTVars req res = do
    let modelTVarsEl = Data.Map.elems modelTVars
    case lookup "Host" (Network.Wai.requestHeaders req) of
        Just "nima.127.0.0.1.xip.io:3000" -> anyRoute2 (modelTVarsEl !! 0)  req res
        Just "cities-temporal.127.0.0.1.xip.io:3000" -> do
            savedFile <- B.readFile "/tmp/cities-temporal.piet"

            modelTVar1 <- atomically $ newTVar (SSS.deserialise savedFile)

            anyRoute2 modelTVar1 req res

        Just "cities.127.0.0.1.xip.io:3000" -> do
            savedFile <- B.readFile "/tmp/cities.piet"

            modelTVar1 <- atomically $ newTVar (SSS.deserialise savedFile)

            anyRoute2 modelTVar1 req res

        Just "load.127.0.0.1.xip.io:3000" -> do
            savedFile <- B.readFile "/tmp/load-me.piet"

            modelTVar1 <- atomically $ newTVar (SSS.deserialise savedFile)

            anyRoute2 modelTVar1 req res

        Just "save.127.0.0.1.xip.io:3000" -> do
            let myTVar = (modelTVarsEl !! 0)
            model <- readTVarIO myTVar

            B.writeFile "/tmp/load-me.piet" (SSS.serialise model)

            anyRoute2 (modelTVarsEl !! 0)  req res
        Just "json.nima.127.0.0.1.xip.io:3000" -> endpointShow (modelTVarsEl !! 0)  "A1" req res
        _ -> anyRoute2 (modelTVarsEl !! 0) req res

anyRoute2 modelTVar req res =
    case pathInfo req of
        [ "minicell", "all.json" ] -> do
            endpointShowAll modelTVar req res

        [ "html", cometKey ] -> do
            endpointShowHTML modelTVar cometKey req res

        [ "minicell", cometKey, "show.json" ] -> do
            endpointShow modelTVar cometKey req res

        ("minicell":cometKey:"HTTP":tail) -> do
            -- TODO: first we must check the content of the cell
            -- We should serve this endpoint only if
            -- the content of the cell is a `=HTTP(arg1)` formula
            -- where `arg1` represents a 2-column region.

            -- We treat this 2-column region as a key-value hash.
            -- where keys map to `tail`, and
            -- values map to the content served at that URL

            model <- readTVarIO modelTVar

            case tail of
                t -> do
                    let needle = case t of
                                    x | x == [] || x == [""] -> "/"
                                    [x] -> "/" ++ (T.unpack x)
                                    _ -> ""

                    print needle

                    columnA <- sequence $ (\x -> do
                        s <- eval model (value x)
                        return $ (x, s)) <$> [ cell | cell <- database model, snd (addr cell) == 0 ]

                    let rowNumberOfslashInColumnA = find (\(_, y) -> y == ESLit needle) columnA

                    print columnA
                    print rowNumberOfslashInColumnA

                    case rowNumberOfslashInColumnA of
                        Nothing -> res $ responseLBS status404 [] ("No default index set for /")
                        Just (indexCell, _) ->  do
                            let (rho, kappa) = addr indexCell
                            indexVal <- eval model (ECellRef (rho, kappa+1))
                            httpResponse <- eexprToHttpResponse indexVal
                            res $ httpResponse

                    -- TODO:if "/" doesn't exist, it's 404, or 403
                _ -> do
                    res $ responseLBS status200
                                          [(hContentType, "application/json")]
                                          (encode $ mconcat $ ["Hello HTTP server at cell ", cometKey, " !", T.pack $ show tail])

        [ "minicell", "purge.json" ] -> do
            atomically $ modifyTVar modelTVar (const emptySpreadsheet)
            res $ responseLBS status200 [(hContentType, "application/json")] (encode $ T.pack "success")

        [ "minicell", cometKey, "write.json" ] -> do
            let cometAddress = (cometKeyToAddr $ T.unpack $ cometKey)



            (params, files) <- parseRequestBody lbsBackEnd req

            let fileParams = case files of
                                [("formula", f)] ->
                                    [("formula", B.toStrict $ fileContent f)]
                                _ -> []
            -- Upload files and get urls
            fileUrls <- storeFiles files

            case (params ++ fileParams) of  -- FIXME: lookup the parameter by name
                ((_,formula):_) -> do

                    let parseRes = parse cellContent "REPL" (BU.toString formula)
                    print parseRes
                    case parseRes of
                        Left err -> do
                            let val = CometSLit cometAddress (show err)
                            res $ responseLBS status200
                                                  [(hContentType, "application/json")]
                                                  (encode val)

                        Right ast -> do
                            -- TODO: update the global database
                            -- TODO: delegate CometSLit transformation to a separate function

                            atomically $ do
                                modifyTVar modelTVar (Mini.modifyModelWithNewCellValue cometAddress ast)

                            endpointShow modelTVar cometKey req res

                _ -> do
                    -- TODO: what if multiple files were dropped on one cell?

                    atomically $ do
                        modifyTVar modelTVar (Mini.modifyModelWithNewCellValue cometAddress (EImage (fileUrls !! 0)))

                    endpointShow modelTVar cometKey req res


        --
        --
        -- Assets
        ["assets", "spreadsheet.js"] -> do

            res $ responseFile status200 [(hContentType, "text/javascript")] ("../build/spreadsheet.js") Nothing

        ("assets":pathToAsset) -> do
            let fullPath = intercalate "/" (T.unpack <$> pathToAsset)
            let myMime = (defaultMimeLookup $ fromString fullPath)
            print myMime
            res $ responseFile status200 [(hContentType, myMime)] ("../static/" <> fullPath) Nothing

        ("minicell-cache":pathToAsset) -> do
            let fullPath = intercalate "/" (T.unpack <$> pathToAsset)
            let myMime = (defaultMimeLookup $ fromString fullPath)
            cachePath <- fullCacheRoot

            let pathToFile = mconcat [cachePath, "/", fullPath]

            res $ responseFile status200 [(hContentType, myMime)] (pathToFile) Nothing

        [] -> do
            let (Just hostName) = requestHeaderHost req
            let messageToUser = mconcat ["Welcome to ", show hostName]

            -- res $ responseFile status200 [(hContentType, "text/html")] "../static/main.html" Nothing
            let (Just mainDotHtml) = Data.List.lookup "main.html" embeddedAssets
            res $ responseLBS status200 [(hContentType, "text/html")] (B.fromStrict mainDotHtml)

        url -> do
            let messageToUser = mconcat [
                                    "Invalid URL " ++ show url, "\n",
                                    "Host:" ++ (show $ requestHeaderHost req), "\n",
                                    show $ Network.Wai.requestHeaders req
                                ]
            res $ responseLBS status404 [(hContentType, "application/json")] (encode $ messageToUser)

