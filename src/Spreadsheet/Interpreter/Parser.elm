module Spreadsheet.Interpreter.Parser exposing (..)

import Spreadsheet.Types exposing (..)
import Parser exposing (Parser, (|.), (|=), succeed, symbol, float, spaces)
import Dict
import Maybe exposing (..)
import List
import Graph exposing (Graph, nodes)
import Result

import String exposing (all)
import Char exposing (isDigit)

import Examples.TopoSort exposing (..)

type alias Point =
  { x : Float
  , y : Float
  }

point : Parser Point
point =
  succeed Point
    |. symbol "("
    |. spaces
    |= float
    |. spaces
    |. symbol ","
    |. spaces
    |= float
    |. spaces
    |. symbol ")"



expr1 = EApp "exampleGraph" [ESLit "dressUp"]

triadGraph = dressUp

graphExamples = Dict.fromList [ ("1", vee), ("2", dressUp) ]


eval : Spreadsheet -> Formula -> Formula

eval model expr = case expr of
  EApp f args -> case f of
                  "exampleGraph" ->
                    let
                      graphIndex = (args |> List.head |> withDefault EBot) |> eval model
                      graphIndexNumber = case graphIndex of
                                          ESLit s -> s
                                          _ -> "dressUp" |> (args |> Debug.toString |> Debug.log)
                    in EGraph (Dict.get graphIndexNumber graphExamples |> withDefault emptyGraph)
                  "+" ->
                    -- what if members of "arg" did not evaluate to EILit? ...
                    case (List.map (eval model) args) of
                      [EILit x, EILit y] -> EILit (x+y)
                      _ -> ESLit "make sure your expressions evaluate to 2 E(xpression)I(integer)Lit(eral)"

                  _ -> ESLit (f ++ " is not implemented")
  ECellRef addr -> case (Dict.get addr model.database |> withDefault emptyCell).value of
                    CellGraph g -> EGraph g
                    CellInt i -> EILit i
                    CellString s -> ESLit s
                    CellFormula subexpr -> eval model subexpr
                    _ -> EBot
  _ -> expr

interpretToCell : Spreadsheet -> Formula -> CellValue
interpretToCell model expr = case expr of
  EGraph g -> CellGraph g
  ESLit s -> CellString s
  EILit i -> CellInt i
  --EApp f args -> CellString "pending computation #{f}" -- "λ"
  EApp f args -> eval model expr |> interpretToCell model
  ECellRef addr -> CellFormula expr
  EBot -> CellString "#ERR"
  --_ -> Debug.log (Debug.toString expr) CellEmpty

stringToEExpr buffer =
  if buffer |> String.startsWith "@" then 
    EApp "exampleGraph" [(buffer |> String.dropLeft 1 |> ESLit)] |> eval emptySpreadsheet
  else if buffer |> String.startsWith "=A" then
    ECellRef (withDefault 0 (buffer |> String.dropLeft 2 |> String.toInt)-1, 0)
  else if buffer |> String.startsWith "=B" then
    ECellRef (withDefault 0 (buffer |> String.dropLeft 2 |> String.toInt)-1, 1)
  else if buffer == "=1+1" then
    EILit 2
  else if all isDigit buffer  then
    EILit (String.toInt buffer |> withDefault 0)
  --else if buffer == "=(+1)" then
  --  ELambda "(+1)"
  else
    ESLit ("I need to learn how to parse " ++ (Debug.toString buffer))

--type Expr = EPlus Expr Expr | EMinus ExprExpr | EAtom Int

--eval expr = case expr of
--  EPlus expr1 expr2 -> (eval expr1) + (eval expr2)
--  EMinus expr1 expr2 -> (eval expr1) - (eval expr2)
--  EAtom e -> e

--arithParser : Parser Expr
--arithParser =
