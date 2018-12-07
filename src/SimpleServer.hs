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

import Data.String

import Spreadsheet.Types

import Spreadsheet.Evaluator.Parser
import Text.ParserCombinators.Parsec


import Network.Wai.Middleware.Cors

type Cache = Int

main = do
    let port = 3000
    cache <- atomically $ newTVar 0

    putStrLn $ "Listening on port " ++ show port
    run port (app cache)

app :: TVar Cache -> Application
app cache =
    websocketsOr  defaultConnectionOptions wsApp (simpleCors $ anyRoute cache)

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

anyRoute cache req res =
    case pathInfo req of
        -- [ "version" ] -> CometString "0.0.1"
        -- parsedUrl -> CometString $ show $ parsedUrl
        [ "minicell", cometKey, "show.json" ] -> do
            counterValue <- atomically $ do
                modifyTVar cache (+1)
                readTVar cache 
            
            print $ pathInfo req

            let val = CometString (cometKeyToAddr $ T.unpack $ cometKey) (show counterValue)

            res $ responseLBS status200
                                  [(hContentType, "application/json")]
                                  (encode val)
        [ "minicell", cometKey, "write.json" ] -> do
            (params, files) <- parseRequestBody lbsBackEnd req
            print (params, length files)
            -- TODO: do something with files!

            let ((_,formula):_) = params -- FIXME: lookup the parameter by name
            
            case parse cellContent "REPL" (BU.toString formula) of 
                Left err -> do
                    let val = CometString (cometKeyToAddr $ T.unpack $ cometKey) (show err)
                    res $ responseLBS status200
                                          [(hContentType, "application/json")]
                                          (encode val)

                Right ast -> do
                    -- TODO: update the global database
                    -- TODO: delegate CometString transformation to a separate function

                    let val = CometString (cometKeyToAddr $ T.unpack $ cometKey) (show ast)
                    res $ responseLBS status200
                                          [(hContentType, "application/json")]
                                          (encode val)


        url ->
            res $ responseLBS status404 [(hContentType, "application/json")] (encode $ "Invalid URL " ++ show url)

