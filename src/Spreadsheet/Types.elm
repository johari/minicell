module Spreadsheet.Types exposing (..)

import Dict
import Graph exposing (Graph, nodes)
import Time exposing (Posix, millisToPosix)

import Examples.TopoSort exposing (dressUp)
-- ^^^^^ This will be removed 

type alias EFunctor = String
type EExpr = EApp EFunctor (List EExpr) -- CellFormula, I guess..
           | EILit Int -- CellInt 
           | ESLit String --CellString
           | ECellRef CellAddress 
           | EBot -- CellEmpty
           | EGraph (Graph String ()) --CellGraph (<3<3<3)
           | EError String
           | EHref String -- CellHref
           -- Later on, types for sound clips, one frame images, animated images, and videos.

type alias Formula = EExpr


type alias CellAddress = ( Int, Int )

type alias CellMeta =
    String


type alias Cell =
    { value : EExpr
    , buffer : String
    , addr : CellAddress
    -- Each time you edit a cell, you are modifying the "buffer". 
    -- Once you press enter the buffer will be parsed, and we replace the "value" attribute with
    -- the result of the parser
    --
    -- the logic for the evaluator can be traced in the code that renders the cells to HTML
    , meta  : Maybe CellMeta
    }

emptyGraph = Graph.fromNodesAndEdges [] []
emptyCell = Cell EBot "" (0, 0) Nothing
stringCell  addr str = { emptyCell | addr = addr, value = ESLit str }
intCell     addr i   = { emptyCell | addr = addr, value = EILit i }
graphCell   addr g   = { emptyCell | addr = addr, value = EGraph emptyGraph }
formulaCell addr formula args = { emptyCell  | addr = addr, value = EApp formula args }

isStringCell cell = case cell.value of
    ESLit _ -> True
    _ -> False

emptySpreadsheet = Spreadsheet [] (IdleMode (0, 0)) [] [] (millisToPosix 0)

type Mode
    = IdleMode CellAddress
    | EditMode CellAddress
    | VertexDemoMode
    | EdgeDemoMode


-- is this necessary?
toString a = case a of
    IdleMode addr -> "IdleMode " ++ (Debug.toString addr)
    EditMode addr -> "EditMode" ++ (Debug.toString addr)
    VertexDemoMode -> "Vertex Demonstration Mode"
    EdgeDemoMode -> "Edge Demonstration Mode"
    --_        -> "Some other mode"


type alias Database = List Cell

type alias Spreadsheet =
    { database : Database
    --, selectionRange : Maybe ( CellAddress, CellAddress )
    , mode : Mode
    , demoVertices : TVertexDemo
    , demoEdges : TEdgeDemo
    , currentTime : Posix
    }

