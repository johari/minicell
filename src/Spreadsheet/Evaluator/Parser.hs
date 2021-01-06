{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Spreadsheet.Evaluator.Parser where

import qualified Control.Exception as E
-- import Piet.DSL.Lua.Hello

-- import Piet.DSL.Media.Flickr

-- import qualified System.IO.Streams as IOS

-- Calendar

-- import Text.ICalendar.Parser
-- import Text.ICalendar.Types

-- Logger

-- Interop

-- import Minicell.Interop.GitHub

-- Tar

-- import Spreadsheet.Evaluator.Tar as PietTar

-- Time stuff

-- Bytestring stuff

-- MD5 stuff

-- Posix stuff

-- Haskell stuff

import Control.Monad
-- Minicell stuff

-- Parsec stuff

-- Stache stuff

import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Char (ord, toLower)
import Data.Digest.Pure.MD5
import Data.List
import qualified Data.Map
import Data.Maybe
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Time.Clock.POSIX
-- import Minicell.Interop.YouTube as YT

-- import Piet.DSL.Debug

-- import Piet.DSL.File
-- import Piet.DSL.Git

-- import Piet.DSL.HTML
-- import Piet.DSL.Lambda

-- import Piet.DSL.Poppet

-- MySQL stuff

-- import Database.MySQL.Simple

-- Shake stuff

import Development.Shake
import Development.Shake.Database
import Development.Shake.FilePath
import Piet.DSL.Arithmetic
import Piet.DSL.FAM
import Piet.DSL.Graphics.PDF
import Piet.DSL.Graphics.Shapes
import Piet.DSL.Graphsheet
import Piet.DSL.List
import Piet.ShakeUtils
import Spreadsheet.Types
import System.Directory
import System.IO
import System.IO.Temp
import System.Log.Logger
import System.Process
import Text.Mustache hiding (Node)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Control.Concurrent.STM

cellContent :: Parser EExpr
cellContent = do
  rest <- getInput
  case rest of
    "" -> return $ EEmpty
    _ -> do
      s <- choice $ [bangBang, formulaWithEqSign, numberLiteral, stringLiteral]
      eof
      return s

bangBang :: Parser EExpr
bangBang = do
  string "!!"
  -- return (EApp "PDF" [ESLit "http://bangbangcon.com/west/images/logo.png", EILit 0])
  return (EApp "PDF" [ESLit "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d2/Reddot-small.svg/240px-Reddot-small.svg.png", EILit 0])
numberLiteral :: Parser EExpr
numberLiteral = do
  num <- many1 digit
  return $ EILit (read num :: Int)

formulaWithEqSign = do
  char '='
  formula

formula = do
  s <-
    choice
      [ try $ formulaCellRef,
        try $ formulaWithOperands,
        try $ numberLiteral,
        try $ stringLiteralInQuotes,
        try $ variableName
      ]
  return s

cometKeyToAddr cometKey =
  case parse excelStyleAddr "" cometKey of
    Right addr -> addr
    Left err -> (-1, -1)

excelStyleAddr :: Parser CellAddress
excelStyleAddr =
  do
    column <- try $ letter
    row <- many1 digit
    return $ (((read row) - 1), ((ord $ toLower $ column) - (ord 'a'))) -- This is ultra buggy (works only for A-F)

-- =A1 or =A1:G7
formulaCellRef = do
  -- column <- try $ many1 letter
  -- row <- try $ many1 digit
  parsedAddress1 <- try $ excelStyleAddr
  rangeRefPortion <- optionMaybe $ do
    try $ string ":"
    parsedAddress2 <- try $ excelStyleAddr
    return $ parsedAddress2

  case rangeRefPortion of
    Just parsedAddress2 -> return $ ECellRange parsedAddress1 parsedAddress2
    Nothing -> return $ ECellRef parsedAddress1

-- =X(A1:G7)
formulaWithOperands = do
  operation <- try $ many1 letter
  char '('
  args <- sepBy formula (char ',' *> spaces)
  char ')'
  return $ EApp operation args

stringLiteral = do
  s <- many1 anyToken
  return $ ESLit s

stringLiteralInQuotes = do
  s <- between (char '"') (char '"') (try $ many1 (noneOf "\""))
  return $ ESLit s

variableName = do
  s <- try $ many1 letter
  return $ EVar s

-- stringToEExpr buffer =
--   if buffer |> String.startsWith "@" then
--     EApp "exampleGraph" [(buffer |> String.dropLeft 1 |> ESLit)] |> eval emptySpreadsheet
--   else if buffer |> String.startsWith "=A" then
--     ECellRef (withDefault 0 (buffer |> String.dropLeft 2 |> String.toInt)-1, 0)
--   else if buffer |> String.startsWith "=B" then
--     ECellRef (withDefault 0 (buffer |> String.dropLeft 2 |> String.toInt)-1, 1)
--   else if buffer == "=1+1" then
--     EILit 2
--   else if all isDigit buffer  then
--     EILit (String.toInt buffer |> withDefault 0)
--   --else if buffer == "=(+1)" then
--   --  ELambda "(+1)"
--   else
--     ESLit (Debug.toString (buffer |> run sexp))

-- Shortcuts to load dummy values (for demo purposes)
--
-- @cities
-- @1
-- @dressUp
-- @2

-- Literals
--
-- Foo
-- 1
--
--
-- =C2
-- =1
-- =SUM(A1:A10)
-- =GUNIOUN(G1:G2)
-- =GUNIOUN(G1, G2)
-- =C3+C1
-- =YOUTUBE("5ScquRuRIyE", 850) (same as =https://www.youtube.com/watch?v=5ScquRuRIyE&t=850s)
-- =YOUTUBE("5ScquRuRIyE", 850, 60)
--
-- Gunrock
-- =GR_LOAD()
-- =GR_UNION()
--
--
-- Perhaps later on we can provide fast vector processing as well
--
-- Or a DSL for combining sound and media
-- or gifs
--
-- It'd be nice if we could connect to external sources and read documents from them too, say
-- git, dat and mysql.
--
-- Sometimes I dream about fetching RSS and JSON as well.
--

pickEvalAndContinue :: (Spreadsheet -> EExpr -> IO EExpr) -> [(Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula] -> Spreadsheet -> Formula -> IO EExpr
pickEvalAndContinue eval [] model expr = evalBot model expr
pickEvalAndContinue eval (eval' : evals) model expr = do
  r <- E.try (eval' eval model expr) :: IO (Either E.SomeException EExpr)
  case r of
    Left e -> do
      print $ e
      (pickEvalAndContinue eval evals model expr)
    Right val -> do
      case val of
        ENotImplemented -> do
          (pickEvalAndContinue eval evals model expr)
        _ -> return val

eval :: Spreadsheet -> EExpr -> IO EExpr
eval model expr = do
  r <- evalTop model expr
  case r of
    ENotImplemented -> do
      let evals =
            [ Piet.DSL.Graphsheet.eval',
              Piet.DSL.List.eval',
              Piet.DSL.Arithmetic.eval',
              Piet.DSL.Graphics.Shapes.eval',
              Piet.DSL.Graphics.PDF.eval',
              -- Piet.DSL.Poppet.eval',
              -- Piet.DSL.Lua.Hello.eval'
              -- Piet.DSL.Debug.eval',
              -- Piet.DSL.Git.eval',
              -- Piet.DSL.HTML.eval',
              -- Piet.DSL.File.eval',
              -- Piet.DSL.Lambda.eval',
              Piet.DSL.FAM.eval'
            ]
      pickEvalAndContinue eval evals model expr
    _ -> return r

{-
eval3 model expr = do
  r <- evalTop model expr
  case r of
    ENotImplemented -> do
      rr <- Piet.DSL.Graphsheet.eval' eval model expr
      case rr of
        ENotImplemented -> do
          rrr <- Piet.DSL.List.eval' eval model expr
          case rrr of
            ENotImplemented -> do
              rrrr <- Piet.DSL.Arithmetic.eval' eval model expr
              case rrrr of
                ENotImplemented -> do
                  rrrrr <- (Piet.DSL.Graphics.Shapes.eval' eval model expr)
                  case rrrrr of
                    ENotImplemented -> do
                      evalBot model expr
                    _ -> return $ rrrrr
                _ -> return $ rrrr
            _ -> return $ rrr
        _ -> return $ rr
    _ -> return $ r
-}

evalBot model expr = case normalizeOp expr of
  EApp op args -> do
    return $ ESLit $ (show op) ++ " " ++ show args ++ " is not implemented"
  _ -> return expr

evalTop model expr = case normalizeOp expr of
  ECellRef lookupAddr -> do
    case find (\x -> addr x == lookupAddr) (database model) of
      Nothing ->
        return $ EError $ "#REF " ++ (addrToExcelStyle lookupAddr)
      Just cell -> eval model (value cell)
  EApp "UNIXEPOCH" _ -> do
    t <- getPOSIXTime
    return $ EILit (round t)
  EApp "BLOB" [blobId] -> do
    let XShakeDatabase db = shakeDatabase model
    case blobId of
      EImage url -> do
        ([EBlob blob], after0) <- shakeRunDatabase db [(askOracle (HttpGet url))]
        shakeRunAfter shakeOptions after0
        return $ EBlob blob
      EBlobId blobId -> do
        let XBlobStorage blobStorageTVar = blobStorage model
        myBlobStorage <- readTVarIO blobStorageTVar
        case Data.Map.lookup blobId myBlobStorage  of
            Just blob -> return (EBlob blob)
            _ -> return (EError "blob ID not found in blob storage")
      _ -> return $ EError (show blobId ++ " was not found in BLOB storage.")
  _ -> return $ ENotImplemented

eval2 :: Spreadsheet -> Formula -> IO Formula
eval2 model expr = case normalizeOp expr of
  EApp "MUSTACHE" args -> do
    let mustacheText = "Hello {{A1}} <a href=\".{{A2}}\">{{A2}}</a>"
    let compiledTemplate = compileMustacheText "foo" mustacheText

    a1InHtml <- eval model (ECellRef (0, 0)) >>= eexprToHtml
    a2InHtml <- eval model (ECellRef (1, 0)) >>= eexprToHtml

    case compiledTemplate of
      Left _ -> return $ EError "Mustache compile failed"
      Right template ->
        return $
          ESLit
            ( L.unpack $
                renderMustache template $
                  object
                    [ "A1" .= (T.pack $ a1InHtml),
                      "A2" .= (T.pack $ a2InHtml)
                    ]
            )

  {-

  EApp "GH" [ repoE ] -> do
    ESLit repo <- eval model repoE
    g <- octoGraph repo
    return (EGraphFGL $ g)

  -}

  EApp "MAKE" args -> do
    case args of
      [ECellRange (rhoL, kappaL) (rhoR, kappaR)] -> do
        let matrix = [(rho, kappa) | rho <- [rhoL .. rhoR], kappa <- [kappaL .. kappaR]]
        vals <- sequence [do val <- eval model (ECellRef addr); return (addr, val) | addr <- matrix]

        return $ ESLit $ intercalate "\n" (curl <$> (filter (\(_, v) -> (eexprToASCII v) /= "") vals))
      _ -> return $ EError "Invalid arguments to MAKE"
    where
      curl (addr, val) =
        mconcat
          [ "curl -X post  http://localhost:3000/minicell/",
            (addrToExcelStyle addr),
            "/write.json -d 'formula=",
            eexprToASCII val,
            "'"
          ]
      eexprToASCII val =
        case val of
          ESLit s -> s
          EILit i -> show i
          EError _ -> ""
          _ -> ""
