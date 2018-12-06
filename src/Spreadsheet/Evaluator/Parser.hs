module Spreadsheet.Evaluator.Parser where

-- Haskell stuff

import Data.List

-- Minicell stuff
import Spreadsheet.Types 
import Spreadsheet.Examples.Graphs

-- Graph stuff

import Data.Graph.Inductive.Query.MaxFlow
import Data.Graph.Inductive.Basic

-- Parsec stuff
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char




cellContent :: Parser EExpr
cellContent = do
  s <- choice $ [ formulaWithEqSign, numberLiteral, stringLiteral ]
  return s

numberLiteral :: Parser EExpr
numberLiteral = do
  num <- many1 digit
  return $ EILit (read num :: Int)

formulaWithEqSign = do
  char '='
  formula

formula = do
  s <- choice [ formulaCellRef, formulaWithOperands ]
  return s

formulaCellRef = do 
  column <- try $ many1 letter
  row <- try $ many1 digit
  return $ ECellRef (0, 0)

formulaWithOperands = do
  operation <- try $ many1 letter
  char '('
  args <- sepBy1 formula (char ',' >> many spaces)
  char ')'
  return $ EApp operation args

stringLiteral = do
  s <- many1 anyToken
  return $ ESLit s
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

eval :: Spreadsheet -> Formula -> IO Formula
eval model expr = case expr of
  ECellRef lookupAddr -> do
    case find (\x -> addr x == lookupAddr) (database model) of
      Nothing ->
        return $ EError "Cell not found"
      Just cell -> eval model (value cell)
  
  
  EApp "reverseEdges" [g] -> do
    -- TODO: Return EError when pattern matching fails

    EGraphFGL g' <- eval model g
    return $ EGraphFGL $ grev g'

  EApp op [ s, t, g ] | op == "maxFlow" || op == "shortestPath" -> do
    -- TODO: Return EError when pattern matching fails

    (ESLit s') <- eval model s
    (ESLit t') <- eval model t
    (EGraphFGL g') <- eval model g

    let nn1 = gsel (\(_, _, label, _) -> label == s') g'
    let nn2 = gsel (\(_, _, label, _) -> label == t') g'

    let ((_, n1, _, _):_) = nn1
    let ((_, n2, _, _):_) = nn2

    case op of
        "maxFlow" -> return (EILit $ maxFlow g' n1 n2)
        -- "shortestPath" -> sp n1 n2 g'
        _ -> return (EError $ "error evaluating " ++ op)


  _ -> return expr
