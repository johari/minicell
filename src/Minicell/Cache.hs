module Minicell.Cache where

--
-- Log


import System.Log.Logger (infoM, setLevel)

--


import System.IO.Temp
import System.Directory
import System.Process


import Data.Digest.Pure.MD5

import Data.String

import Network.Wreq
import Control.Lens

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL

type CacheKey = (String, String)

doesObjectExist :: CacheKey -> IO Bool
doesObjectExist cacheKey = fullPathForKey cacheKey >>= doesFileExist


pathForKey key = mconcat [(fst key), "/", (snd key)]

fullPathForKey (ns, key) = do
    cachePath <- cachePathFor ns
    return $ mconcat $ [cachePath, "/", key]

fullCacheRoot = do
    h <- getHomeDirectory
    return $ h ++ "/.cache/minicell"

-- e.g. storeObject (mconcat [tmp, "foo-", (show page), ".png"])
storeObject :: CacheKey -> B.ByteString -> IO FilePath -- FIXME: return success/failure
storeObject cacheKey@(ns, objectId) content = do
    finalDestination <- fullPathForKey cacheKey

    infoM "wiki.sheets.cache" ("writing to " ++ show finalDestination)

    B.writeFile finalDestination content
    return finalDestination

cachePathFor namespace = do
    root <- fullCacheRoot

    infoM "wiki.sheets.cache" ("full cache root is " ++ show root)

    let dirName = mconcat [root, "/", namespace]
    print (dirName, length dirName)
    huh <- doesDirectoryExist dirName
    case huh of
        True -> return ()
        False -> do
            createDirectory dirName
    return $ dirName

-- cachePathFor keyName = do
--     h <- getHomeDirectory
--     return (h ++ "/Dropbox/minicell-uploads/" ++ keyName ++ "/")


retrieveAndStoreUrlToFile :: String -> IO (String, String)

retrieveAndStoreUrlToFile url = do
    -- Consult the cache
    -- Checksum of URL


    let md5Digest = show $ md5 (BL.pack url)

    let cacheKey = ("http", md5Digest)

    targetPath <- fullPathForKey cacheKey

    fileExist <- doesObjectExist cacheKey

    case fileExist of
        True -> return $ (md5Digest, targetPath)
        False -> do
            r <- get url
            let resBody = r ^. responseBody

            BL.writeFile targetPath resBody

            return $ (md5Digest, targetPath)


{-
runReq defaultHttpConfig $ do
  let payload = object
        [ "foo" .= (10 :: Int)
        , "bar" .= (20 :: Int) ]
  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  r <- req POST -- method
    (https "httpbin.org" /: "post") -- safe by construction URL
    (ReqBodyJson payload) -- use built-in options or add your own
    jsonResponse -- specify how to interpret response
    mempty       -- query params, headers, explicit port number, etc.
  liftIO $ print (responseBody r :: Value)

-}
