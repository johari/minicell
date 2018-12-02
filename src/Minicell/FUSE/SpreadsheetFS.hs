module Minicell.FUSE.SpredsheetFS where


-- import System.Log.Logger
-- import System.Log.Handler.Syslog
-- import System.Log.Handler.Simple
-- import System.Log.Handler (setFormatter)
-- import System.Log.Formatter

import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

import System.Process
import System.Posix.Process

import System.Fuse

import Minicell.FUSE.RouteParser

import Text.ParserCombinators.Parsec (parse)


type HT = ()

main :: IO ()
main = do
  pid <- getProcessID
  writeFile "/tmp/SpreadsheetFS.pid" (show pid)
  case (parse cellAddressPath "" "/A25.cell") of
    Left err -> print err
    Right xs -> print xs

  -- h <- fileHandler "/tmp/SpreadsheetFS.log" DEBUG >>= \lh -> return $
  --                 setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  -- updateGlobalLogger "MyApp.BuggyComponent" (addHandler h)
  -- debugM "MyApp.BuggyComponent" "Some useful diagnostics..."

  fuseMain helloFSOps defaultExceptionHandler


fuseLog component str = do
  createProcess (proc "notify-send" [component, str])

-- "open" syscall

helloOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
helloOpen path mode flags
    | path == helloPath = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES) -- TODO: implement write access :)
    | otherwise         = return (Left eNOENT)


-- "read" a path

takeAndDrop byteCount offset bs = B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) bs

helloRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
helloRead path _ byteCount offset
    | path == helloPath = do
        fuseLog "helloRead" (path ++ " has a new visitor! :)")
        return $ Right $ takeAndDrop byteCount offset helloString
    | otherwise         =
        case (parse cellAddressPath "" path) of
          Right addr -> return $ Right $ takeAndDrop byteCount offset (B.pack $ show addr)
          Left err -> return $ Left eNOENT

-- STAT

helloGetFileStat :: FilePath -> IO (Either Errno FileStat)
helloGetFileStat "/" = do
    ctx <- getFuseContext
    return $ Right $ dirStat ctx

helloGetFileStat path | path == helloPath = do
    ctx <- getFuseContext
    return $ Right $ fileStat ctx

helloGetFileStat path = do
    fuseLog "stat" (path ++ " not found!")
    return $ Left eNOENT

--  OPEN DIRECTORY

helloOpenDirectory "/" = return eOK
helloOpenDirectory _   = return eNOENT

helloReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
helloReadDirectory "/" = do
    ctx <- getFuseContext
    return $ Right [(".",          dirStat  ctx)
                   ,("..",         dirStat  ctx)
                   ,(helloName,    fileStat ctx)
                   ]
    where (_:helloName) = helloPath
helloReadDirectory _ = return (Left (eNOENT))

-- LITERALS

helloString :: B.ByteString
helloString = B.pack "Hello Duke, from Haskell!!\n"

helloPath :: FilePath
helloPath = "/hello.html"


-- Low-level stuff
-- Copy/pasted from the internet :)

helloFSOps :: FuseOperations HT
helloFSOps = defaultFuseOps { fuseGetFileStat = helloGetFileStat
                            , fuseOpen        = helloOpen
                            , fuseRead        = helloRead 
                            , fuseOpenDirectory = helloOpenDirectory
                            , fuseReadDirectory = helloReadDirectory
                            , fuseGetFileSystemStats = helloGetFileSystemStats
                            }
dirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

fileStat ctx = FileStat { statEntryType = RegularFile
                        , statFileMode = foldr1 unionFileModes
                                           [ ownerReadMode
                                           , groupReadMode
                                           , otherReadMode
                                           ]
                        , statLinkCount = 1
                        , statFileOwner = fuseCtxUserID ctx
                        , statFileGroup = fuseCtxGroupID ctx
                        , statSpecialDeviceID = 0
                        , statFileSize = fromIntegral $ B.length helloString
                        , statBlocks = 1
                        , statAccessTime = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0
                        }


helloGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
helloGetFileSystemStats str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }
