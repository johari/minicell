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

import Control.Concurrent.STM

import Control.Monad.IO.Class (liftIO)

import Data.Monoid
import Data.String

import Spreadsheet.Types
import Spreadsheet as Mini

import Spreadsheet.Evaluator.Parser
import Text.ParserCombinators.Parsec

import Network.Wai.Middleware.Cors

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


-- The data that will be converted to JSON
-- jsonData = ["a","b","c"] :: [Text]
dummyJsonData :: CometValue
dummyJsonData = CometAddr (0, 0)

anyRoute modelTVar req res =
    case pathInfo req of
        [ "minicell", cometKey, "show.json" ] -> do
            let cometAddress = (cometKeyToAddr $ T.unpack $ cometKey)
            -- counterValue <- atomically $ do
            --     modifyTVar modelTVar (+1)
            --     readTVar modelTVar 

            val <- do
                model <- readTVarIO modelTVar
                res <- eval model (ECellRef cometAddress)

                -- This is the main point of integration betweenR
                -- A) Haskell values
                -- B) Elm values
                -- C) Transforming unusual values to something suitable to render in Frontend


                case res of
                    EGraphFGL g -> do
                        let dot = showDot (fglToDot g)
                        let dotPath = "../build/minicell-cache/file.dot"
                        let pngPath = "../build/minicell-cache/file.png"
                        writeFile dotPath dot
                        system ("dot -Tpng -o" ++ pngPath ++ " " ++ dotPath)

                        return $ CometImage cometAddress "/minicell-cache/file.png"
                    -- EIMage g -> -- send the src
                    _ -> return $ CometSLit cometAddress (show res)
                
                -- return $ CometSLit cometAddress (show res)

            -- let val = CometSLit  (mconcat ["Hello ", show cometKey, " from Haskell!"])

            res $ responseLBS status200
                                  [(hContentType, "application/json")]
                                  (encode val)
        [ "minicell", cometKey, "write.json" ] -> do
            let cometAddress = (cometKeyToAddr $ T.unpack $ cometKey)

            (params, files) <- parseRequestBody lbsBackEnd req
            print (params, length files)
            -- TODO: do something with files!

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

                    model <- readTVarIO modelTVar
                    valueOfAst <- eval model ast
    
                    let val = case valueOfAst of
                                ESLit s -> CometSLit (cometKeyToAddr $ T.unpack $ cometKey) s
                                _ -> CometSLit (cometKeyToAddr $ T.unpack $ cometKey) (show valueOfAst)

                    res $ responseLBS status200
                                          [(hContentType, "application/json")]
                                          (encode val)


        url ->
            res $ responseLBS status404 [(hContentType, "application/json")] (encode $ "Invalid URL " ++ show url)

