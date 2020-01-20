{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TupleSections #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


{-# LANGUAGE ScopedTypeVariables #-}


module Spreadsheet.Evaluator.Parser where


import Data.Char (ord, toLower)

import Piet.DSL.Arithmetic
import Piet.DSL.List
import Piet.DSL.Graphsheet
import Piet.DSL.Graphics.Shapes
import Piet.DSL.HTML
import Piet.DSL.Poppet
-- import Piet.DSL.Lua.Hello
import Piet.DSL.Debug
import Piet.DSL.Git
import Piet.DSL.FAM
import Piet.DSL.File
import Piet.DSL.Lambda
-- import Piet.DSL.Media.Flickr

-- import qualified System.IO.Streams as IOS
import System.IO


-- Calendar

-- import Text.ICalendar.Parser
-- import Text.ICalendar.Types

-- Logger

import System.Log.Logger

-- Interop

-- import Minicell.Interop.GitHub
import Minicell.Interop.PDF as PDF
import Minicell.Interop.YouTube as YT

-- Tar

-- import Spreadsheet.Evaluator.Tar as PietTar

-- Time stuff

import Data.Time.Clock.POSIX


-- Bytestring stuff

import qualified Data.ByteString.Lazy as LB

-- MD5 stuff

import Data.Digest.Pure.MD5

-- Posix stuff

import System.IO.Temp
import System.Directory
import System.Process

-- Haskell stuff

import Data.List
import Data.Maybe
import qualified Data.Map

import Control.Monad

-- Minicell stuff
import Spreadsheet.Types


-- Parsec stuff
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char


-- Stache stuff

import Data.String
import Data.Aeson
import Text.Mustache hiding (Node)
import Data.Aeson
import qualified Data.Text.Lazy as L
import qualified Data.Text as T

-- MySQL stuff

import Database.MySQL.Simple

cellContent :: Parser EExpr
cellContent = do
  rest <- getInput
  case rest of
    "" -> return $ EEmpty
    _ -> do
        s <- choice $ [ bangBang, formulaWithEqSign, numberLiteral, stringLiteral ]
        eof
        return s

bangBang :: Parser EExpr
bangBang = do
  string "!!"
  return (EApp "PDF" [ESLit "http://bangbangcon.com/west/images/logo.png", EILit 0])

numberLiteral :: Parser EExpr
numberLiteral = do
  num <- many1 digit
  return $ EILit (read num :: Int)

formulaWithEqSign = do
  char '='
  formula

formula = do
  s <- choice [ try $ formulaCellRef
              , try $ formulaWithOperands
              , try $ numberLiteral
              , try $ stringLiteralInQuotes
              , try $ variableName
              ]
  return s

cometKeyToAddr cometKey =
  case parse excelStyleAddr "" cometKey of
    Right addr -> addr
    Left err  -> (-1, -1)

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


pickEvalAndContinue eval [] model expr = evalBot model expr
pickEvalAndContinue eval (eval':evals) model expr = do
  r <- eval' eval model expr
  case r of
    ENotImplemented -> do

      (pickEvalAndContinue eval evals model expr)
    _ -> return r

eval model expr = do
  r <- evalTop model expr
  case r of
    ENotImplemented -> do
      let evals =
            [ Piet.DSL.HTML.eval'
            , Piet.DSL.Graphsheet.eval'
            , Piet.DSL.List.eval'
            , Piet.DSL.Arithmetic.eval'
            , Piet.DSL.Graphics.Shapes.eval'
            , Piet.DSL.Poppet.eval'
            -- , Piet.DSL.Lua.Hello.eval'
            , Piet.DSL.Debug.eval'
            , Piet.DSL.Git.eval'
            , Piet.DSL.FAM.eval'
            , Piet.DSL.File.eval'
            , Piet.DSL.Lambda.eval'
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
        return $ ESLit (L.unpack $ renderMustache template $ object [ "A1" .= (T.pack $ a1InHtml)
                                                                    , "A2" .= (T.pack $ a2InHtml)
                                                                    ])


  {-

  EApp "GH" [ repoE ] -> do
    ESLit repo <- eval model repoE
    g <- octoGraph repo
    return (EGraphFGL $ g)

  -}




  EApp "MAKE" args -> do
    case args of
      [ECellRange (rhoL, kappaL) (rhoR, kappaR)] -> do
        let matrix = [ (rho, kappa) | rho <- [rhoL .. rhoR], kappa <- [kappaL .. kappaR] ]
        vals <- sequence [ do val <- eval model (ECellRef addr); return (addr, val) | addr <- matrix ]

        return $ ESLit $ intercalate "\n" (curl <$> (filter (\(_,v) -> (eexprToASCII v) /= "") vals))

      _ -> return $ EError "Invalid arguments to MAKE"

    where
      curl (addr, val) = mconcat ["curl -X post  http://localhost:3000/minicell/"
                                 , (addrToExcelStyle addr)
                                 , "/write.json -d 'formula="
                                 , eexprToASCII val
                                 , "'" ]
      eexprToASCII val =
        case val of
          ESLit s -> s
          EILit i -> show i
          EError _ -> ""
          _ -> ""


