-- See: https://github.com/jwiegley/gitlib/tree/master/gitlib
-- https://twitter.com/NimaJohari/status/1172982201815683072
-- http://hackage.haskell.org/package/gitlib-3.1.2/docs/Git-Tutorial.html
-- https://github.com/jwiegley/gitlib/tree/master/gitlib

{-# Language OverloadedStrings #-}

module Piet.DSL.Git where

import Spreadsheet.Types

import Git
import Git.Libgit2 (lgFactory)
import Control.Monad.IO.Class
import Data.Tagged
import Data.Time
import qualified Data.Text as T

import Data.Tagged

repoOpts = RepositoryOptions { repoPath = "../../oopsla19/"
                             , repoWorkingDir = Nothing
                             , repoIsBare = False
                             , repoAutoCreate = False
                             }


eval' :: (Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula
eval' eval model expr = case normalizeOp expr of
  EApp "README" [ _ ] -> do
    res <- loadREADME
    return res

  _ -> return $ ENotImplemented

-- headTree :: IO (Tree Git.Libgit2.Types.LgRepo)
headTree = do
  withRepository' lgFactory repoOpts $ do
    repo <- getRepository
    -- Just refTarget <- lookupReference "HEAD"
    Just refTarget <- lookupReference "refs/heads/master"

    let textOid = case refTarget of
                      RefObj oid -> renderOid oid
                      RefSymbolic refName -> error $ "could figure out HEAD.. wtf is " <> (show refName)

    -- t <- (parseObjOid "8834408a2f")
    t <- (parseObjOid $ textOid)
    -- let (RefObj t) = untag refTarget

    commitFromRef <- lookupCommit t
    liftIO $ print $ commitAuthor commitFromRef
    liftIO $ print $ commitLog commitFromRef

    liftIO $ print $ commitTree commitFromRef

    t <- lookupTree (commitTree commitFromRef)

    return t

loadREADME :: IO EExpr
loadREADME = do
    t <- headTree
    withRepository' lgFactory repoOpts $ do
      Just tt <- treeEntry t "README.md"
      let myOid = treeEntryToOid tt

      obj <- lookupObject myOid

      case obj of
        BlobObj blob -> do
          case blobContents blob of
            BlobString s -> liftIO $ return $ ESLit $ show s
            _ -> liftIO $ return $ EError "can't parse blob"
        _ -> liftIO $ return $ EError "Object is not "

main = do
  s <- loadREADME
  print s