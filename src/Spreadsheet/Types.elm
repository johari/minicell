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
    -- Each time you edit a cell, you are modifying the "buffer". 
    -- Once you press enter the buffer will be parsed, and we replace the "value" attribute with
    -- the result of the parser
    --
    -- the logic for the evaluator can be traced in the code that renders the cells to HTML
    , meta  : Maybe CellMeta
    }

emptyCell = Cell EBot "" Nothing
stringCell str = { emptyCell | value = ESLit str }
intCell i = { emptyCell | value = EILit i }
graphCell g = { emptyCell | value = EGraph dressUp }
formulaCell formula args = { emptyCell | value = EApp formula args }

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
