module Spreadsheet.Interpreter.Parser exposing (..)

import Spreadsheet.Types exposing (..)
import Parser exposing (..)
import Dict
import Maybe exposing (..)
import List
import List.Extra exposing (find)
import Graph exposing (Graph, nodes)
import Result

import String exposing (all)
import Char exposing (isDigit)

import Examples.TopoSort exposing (..)

import Time exposing (posixToMillis)

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

graphExamples = Dict.fromList [] -- [ ("1", vee), ("2", dressUp) ]


eval : Spreadsheet -> Formula -> Formula

eval model expr = case expr of
  EApp f args -> case f of
                  "secondsSinceEpoch" -> model.currentTime |> posixToMillis |> EILit
                  "exampleGraph" ->
                    let
                      graphIndex = (args |> List.head |> withDefault EBot) |> eval model
                      graphIndexNumber = case graphIndex of
                                          ESLit s -> s
                                          _ -> "self"
                    in
                      --if True || graphIndexNumber == "self" then
                        let nodes = model.database |> List.filter isStringCell |> (List.map (\cell -> { cell | value = eval model cell.value })) |> List.indexedMap Graph.Node
                            edges = [Graph.Edge 1 2 ()]
                        in
                          Debug.log (Debug.toString nodes) (ECellGraph (Graph.fromNodesAndEdges nodes edges))
                      --else
                      --  EGraph (Dict.get graphIndexNumber graphExamples |> withDefault emptyGraph)
                  "+" ->
                    -- what if members of "arg" did not evaluate to EILit? ...
                    case (List.map (eval model) args) of
                      [EILit x, EILit y] -> EILit (x+y)
                      _ -> ESLit "make sure your expressions evaluate to 2 E(xpression)I(integer)Lit(eral)"

                  _ -> ESLit (f ++ " is not implemented")
  ECellRef addr -> ((find (\x -> x.addr == addr) model.database) |> withDefault emptyCell).value
  _ -> expr

digits = number {
    int = Just EILit
  , hex = Just EILit
  , octal = Just EILit
  , binary = Just EILit
  , float = Nothing
  }

identifier = succeed (\_ -> ESLit "ident") |= (keyword "ident")

atomic = oneOf [ digits ]

--sexp = oneOf
--        [ digits
--        --, quotedList
--        --, succeed identity
--        --    |. symbol "("
--        --    |= identifier
--        --    --|= loop [] ((|= spaces) |> andThen (lazy (\_ -> sexp)) |> andThen (|= spaces))
--        --    |. symbol ")"
--        ]

--sexpHelp : List EExpr -> Parser (Step (List EExpr) (List EExpr))
--sexpHelp revStmts =
--  oneOf
--  [ succeed (\stmt -> Done [stmt]) |= identifier
--  , succeed (\stmt -> Loop (stmt :: revStmts))
--      |= identifier
--      |. spaces
--      |. symbol ";"
--      |. spaces
--  , succeed ()
--      |> Parser.map (\_ -> Done (List.reverse revStmts))
--  ]

--quotedList = succeed identity
--              |. symbol "'(" 
--              |= loop [] sexpHelp
--              |. symbol ")"

sexp = sequence { start = "(", separator = ",", end = ")", spaces = spaces, item = identifier, trailing = Forbidden } |. end

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
    ESLit (Debug.toString (buffer |> run sexp))

--type Expr = EPlus Expr Expr | EMinus ExprExpr | EAtom Int

--eval expr = case expr of
--  EPlus expr1 expr2 -> (eval expr1) + (eval expr2)
--  EMinus expr1 expr2 -> (eval expr1) - (eval expr2)
--  EAtom e -> e

--arithParser : Parser Expr
--arithParser =
