{-# Language OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Aeson hiding (json)

import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    -- model <- IORef emptyModel

    scotty 3000 $ do
        get "/document/:documentName/dump.json" $ do
            json $ object []
            -- Typically called when Elm wants to load a new sheet

            
            -- TODO:
            -- dump the entire spreadsheet stored inside model and send it as JSON

        get "/document/:documentName/:row/:column.json" $ do
            -- Probably wouldn't be used as much given we have "dump.json"
            json $ object [ "ok" .= ("ok" :: String) ]

        -- One of the most important functions in the backend
        -- 💖
        post "/document/:documentName/:row/:column.json" $ do
            liftIO $ putStrLn "Elm sent an update request containing this payload"
            json $ object [ "ok" .= ("ok" :: String) ]
            -- Respond to Elm's edit mode

            -- Step 1: Parse formula
            -- Step 2: Deicde if it's valid or not
                -- VALID:
                -- Store it in the model and serialize an EExpr and send back as JSON

                -- INVALID:
                -- Send an (EError String) in form of a JSON where the String value is parsec's error message
                -- (Bonus: The interface inside Elm could specialize on error message and provide further guidance to the user)
            -- Done

    
