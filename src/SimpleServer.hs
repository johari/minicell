{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import qualified Data.Text as T

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Network.Wai.Parse

import Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B


import Control.Concurrent.STM

import Control.Monad.IO.Class (liftIO)

-- Minicell stuff

import Spreadsheet.Types
import Spreadsheet as Mini

import Spreadsheet.Evaluator.Parser
import Text.ParserCombinators.Parsec

-- Graph stuff

import Data.Graph.Inductive.Dot

import System.Process

main = do
    let port = 3000
    modelTVar <- atomically $ newTVar emptySpreadsheet

    putStrLn $ "Listening on port " ++ show port
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


eexprToComet model cometAddress  = do
    cellValue <- eval model (ECellRef cometAddress)

    -- This is the main point of integration betweenR
    -- A) Haskell values
    -- B) Elm values
    -- C) Transforming unusual values to something suitable to render in Frontend


    case cellValue of
        EGraphFGL g -> do
            let dot = showDot (fglToDot g)
            let dotPath = "../build/minicell-cache/file.dot"
            let pngPath = "../build/minicell-cache/file.png"
            writeFile dotPath dot
            system ("dot -Tpng -o" ++ pngPath ++ " " ++ dotPath)

            return $ CometImage cometAddress "/minicell-cache/file.png"
        ESLit s -> return $ CometSLit cometAddress s
        EILit i -> return $ CometILit cometAddress i
        EImage src -> return $ CometImage cometAddress src
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

    B.writeFile ("/Users/nima/Dropbox/minicell-uploads/" ++ (uploadedFileName)) uploadedContent

    otherUrls <- storeFiles xs

    return (["http://localhost:8001/" ++ (uploadedFileName)] ++ otherUrls)

anyRoute modelTVar req res =
    case pathInfo req of
        [ "minicell", "all.json" ] -> do
            endpointShowAll modelTVar req res

        [ "minicell", cometKey, "show.json" ] -> do
            endpointShow modelTVar cometKey req res

        [ "minicell", cometKey, "write.json" ] -> do
            let cometAddress = (cometKeyToAddr $ T.unpack $ cometKey)


            (params, files) <- parseRequestBody lbsBackEnd req

            -- Upload files and get urls
            fileUrls <- storeFiles files
            print (params, fileUrls)

            case files of
                [] -> do
                    let ((_,formula):_) = params -- FIXME: lookup the parameter by name

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

