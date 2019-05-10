{-# LANGUAGE OverloadedStrings #-}

module Minicell.Interop.YouTube where

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


magicYT videoId = do 
    -- Cache key for the video
    let cacheKey =  ("minicell-yt", videoId)

    fileExist <- doesObjectExist cacheKey

    -- Return the address so that cells can render it
    case fileExist of 
        True -> return ()
        _ -> withSystemTempDirectory "minicell-yt" $ \tmp -> do

                -- IMPORTANT
                -- FIXME: We should make sure we construct this line in a safe way, otherwise this might be exploited in case of malicous user inputs
                -- IMPORTANT
                readProcess "youtube-dl" [ ("https://www.youtube.com/watch?v=" <> videoId)
                                         , "-o" , "output"
                                         ] ""

                content <- B.readFile (mconcat [tmp, "output"])
                storeObject cacheKey content
                return ()

    return (pathForKey cacheKey)