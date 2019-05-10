{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Logger stuff

import System.Log.Logger (Priority (DEBUG), debugM, infoM, setLevel,
                          updateGlobalLogger, warningM, noticeM,
                          rootLoggerName,
                          setHandlers
                          )
import System.Log.Handler.Color
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

-- Bytestring stuff

import Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS


import Control.Concurrent.STM

import Control.Monad.IO.Class (liftIO)

-- Minicell stuff

import Spreadsheet.Types
import Spreadsheet as Mini

import Spreadsheet.Evaluator.Parser
import Text.ParserCombinators.Parsec

-- Graph stuff

import Data.Graph.Inductive.Dot
import Data.Graph.Inductive.Graph

import System.Process

import System.IO (stderr)

main = do
    let port = 3000
    modelTVar <- atomically $ newTVar emptySpreadsheet

    -- updateGlobalLogger rootLoggerName (setFormatter $ simpleLogFormatter "[$loggername] $msg")
    h <- streamHandler stderr DEBUG >>= \lh -> return $
                    setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")

    updateGlobalLogger rootLoggerName (setHandlers [h])

    -- updateGlobalLogger "wiki.sheets.http" (setLevel DEBUG)
    updateGlobalLogger "wiki.sheets.eval.eapp" (setLevel DEBUG)

    infoM "wiki.sheets.http" ("Listening on port " ++ show port)

    run port (app modelTVar)

app :: TVar Spreadsheet -> Application
app modelTVar =
    websocketsOr  defaultConnectionOptions wsApp (simpleCors $ anyRoute modelTVar)

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


eexprToComet model cometAddress  = do
    cellValue <- eval model (ECellRef cometAddress)

    -- This is the main point of integration betweenR
    -- A) Haskell values
    -- B) Elm values
    -- C) Transforming unusual values to something suitable to render in Frontend

    let dotPath = "../build/minicell-cache/" ++ (addrToExcelStyle cometAddress) ++ ".dot"
    let pngPath = "../build/minicell-cache/" ++ (addrToExcelStyle cometAddress) ++ ".png"
    let svgPath = "../build/minicell-cache/" ++ (addrToExcelStyle cometAddress) ++ ".svg"

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
        EILit i -> return $ CometILit cometAddress i
        EImage src -> return $ CometImage cometAddress ("/minicell-cache/" <> src)
        EVideo src -> return $ CometVideo cometAddress src
        _ -> return $ CometSLit cometAddress (show cellValue)


endpointShow modelTVar cometKey req res = do
    let cometAddress = (cometKeyToAddr $ T.unpack $ cometKey)

    model <- readTVarIO modelTVar
    val <- eexprToComet model cometAddress
        

    res $ responseLBS status200
                          [(hContentType, "application/json")]
                          (encode val)

endpointShowAll modelTVar req res = do
    model <- readTVarIO modelTVar

    let existingKeys = (database model)

    allOfCells <- sequence $ eexprToComet model <$> addr <$> (database model)

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

anyRoute modelTVar req res =
    case pathInfo req of
        [ "minicell", "all.json" ] -> do
            endpointShowAll modelTVar req res

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

                    print formula

                    case parse cellContent "REPL" (BU.toString formula) of 
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


        url ->
            res $ responseLBS status404 [(hContentType, "application/json")] (encode $ "Invalid URL " ++ show url)

