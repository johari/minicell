module Piet.DSL.File where

import System.Directory
import Spreadsheet.Types

eval' :: (Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula
eval' eval model expr = case normalizeOp expr of

  EApp "DIR" _ -> do
    -- path <- eval model pathE
    -- print path
    listOfFiles <- getDirectoryContents "/minibox/"
    print listOfFiles
    return $ EList (EImage <$> listOfFiles)

  EApp "FILE" [ pathE ] -> do
    p <- eval model pathE
    case p of
        ESLit path -> do
                exists <- doesFileExist path
                if exists then do
                    contens <- readFile path
                    return $ ESLit contens
                else
                    return $ (EError "file does not exist")
        _ -> return $ EError "Please pass a string for path :)"

  EApp "HAXL" [ pathE ] -> do
    p <- eval model pathE
    case p of
        ESLit path -> do
                exists <- doesFileExist path
                if exists then do
                    contens <- readFile path
                    return $ ESLit contens
                else
                    return $ (EError "file does not exist")
        _ -> return $ EError "Please pass a string for path :)"

  _ -> return ENotImplemented

  -- EApp "TAR" _ -> do
  --   PietTar.example
  --   return $ EString "success!"
