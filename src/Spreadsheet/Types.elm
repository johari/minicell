module Spreadsheet.Types exposing (..)

import Dict
import Graph exposing (Graph, nodes)
import Time exposing (Posix, millisToPosix)

import Examples.TopoSort exposing (dressUp)
-- ^^^^^ This will be removed 

type alias EFunctor = String
type EExpr = EApp EFunctor (List EExpr) | EILit Int | ESLit String | ECellRef CellAddress | EBot | EGraph (Graph String ())

type alias Formula = EExpr


type alias CellAddress = ( Int, Int )


type CellValue
    = CellEmpty -- e.g. ()

    | CellInt Int -- e.g. 42
    
    -- <3 <3 <3
    | CellGraph (Graph String ()) -- e.g. G = <V, E>
    -- <3 <3 <3

    | CellString String -- e.g. "Hello World!"
    | CellFormula Formula -- e.g. =Dijkstra(G1, "Davis", "Berkeley")
    | CellHref String -- e.g. http://cs.tufts.edu/~nr/...
    | HCellList CellValue (List CellValue) -- e.g. Shouldn't this be "HCellList (List CellValue)" instead?

type alias CellMeta =
    String


type alias Cell =
    { value : CellValue
    , buffer : String 
    -- Each time you edit a cell, you are modifying the "buffer". 
    -- Once you press enter the buffer will be parsed, and we replace the "value" attribute with
    -- the result of the parser
    --
    -- the logic for the evaluator can be traced in the code that renders the cells to HTML
    , meta  : Maybe CellMeta
    }

emptyCell = Cell CellEmpty "" Nothing
stringCell str = { emptyCell | value = CellString str }
intCell i = { emptyCell | value = CellInt i }
graphCell g = { emptyCell | value = CellGraph dressUp }
formulaCell formula = { emptyCell | value = CellFormula formula }

emptySpreadsheet = Spreadsheet (Dict.fromList []) (IdleMode) (Nothing) (Just (0,0)) [] [] (millisToPosix 0)

type Mode
    = IdleMode
    | EditMode
    | VertexDemoMode
    | EdgeDemoMode


-- is this necessary?
toString a = case a of
    IdleMode -> "IdleMode"
    EditMode -> "EditMode"
    VertexDemoMode -> "Vertex Demonstration Mode"
    EdgeDemoMode -> "Edge Demonstration Mode"
    --_        -> "Some other mode"


type alias Database = Dict.Dict CellAddress Cell

type alias Spreadsheet =
    { database : Database
    --, selectionRange : Maybe ( CellAddress, CellAddress )
    , mode : Mode
    , cellUnderModification : Maybe CellAddress
    , cellUnderView : Maybe CellAddress
    , demoVertices : List CellAddress
    , demoEdges : List (CellAddress, CellAddress)
    , currentTime : Posix
    }
