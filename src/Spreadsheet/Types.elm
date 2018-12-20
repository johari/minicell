module Spreadsheet.Types exposing (..)

import Tuple
import Dict
import Graph exposing (Graph, nodes)
import Time exposing (Posix, millisToPosix)
import List exposing (any)

type alias Demonstrations = { vertexDemos : List VertexAndPerhapsCells, edgeDemos : TEdgeDemo }
type alias Situation = { database : Database, demos : Demonstrations}

emptyDemonstration = { vertexDemos = [], edgeDemos = [] }

type alias Wrangler = (Database -> G)
type alias Score = Int

type alias VertexAndPerhapsCells = (EExpr, List Cell)
type alias EdgeLabel = (Maybe EExpr)
type alias G = Graph (VertexAndPerhapsCells) EdgeLabel

type alias SuperEdge = (VertexAndPerhapsCells, VertexAndPerhapsCells, EdgeLabel)

type alias TVertexDemo = List VertexAndPerhapsCells


getCellVertex : CellAddress -> TVertexDemo -> List VertexAndPerhapsCells
getCellVertex addr demoOfVertices = 
     demoOfVertices
  |> List.filter (\(_, cells) -> List.map (\x -> x.addr == addr) cells |> any identity)

addrInVertexDemo addr demoOfVertices =
  (getCellVertex addr demoOfVertices |> List.length) > 0

removeAddrFromDemoVertices : CellAddress -> TVertexDemo -> TVertexDemo
removeAddrFromDemoVertices addr demoOfVertices = 
     demoOfVertices
  |> List.filter
      (\(_, cells) ->
        List.map (\x -> x.addr == addr) cells |> any not)

type alias TEdgeDemo   = List SuperEdge

type alias EFunctor = String

type EExpr = EApp EFunctor (List EExpr) -- CellFormula, I guess..
           | EILit Int -- CellInt 
           | ESLit String --CellString
           | ECellRef CellAddress 
           | EBot -- CellEmpty
           
           | EEmpty

           | ECellGraph (Graph Cell ())
           | ESuperFancyGraph G
           -- ^^^ A super fancy type
           -- that allow you to jump from one cell to another
           -- if cells are linked with respect to a graph (normally graph inside the cellUnderView)

           | EGraph (Graph String ()) -- Good old "CellGraph" (<3<3<3)
           | EGraphFromDemo TVertexDemo TEdgeDemo
           | EError String
           | EHref String -- CellHref

           -- [ ] one frame images
           --       [ ] basic values:
           --             read from file
           --       [ ] basic operations:
           --             whatever "convert" can do (imageMagik)

           | EImage String -- url
           | EYouTube { video_id : String, start : Maybe String, end : Maybe String }

           | EComet CometKey

           -- [ ] animated images (gif images)
           --       [ ] basic values:
           --             read from file (or URL)
           --       [ ] basic operations
           --             things Duke mentioned in his system (w.r.t. combining gif images)  

           -- Later on, types for
           -- [ ] sound clips,
           --       [ ] basic values:
           --             sound of silence (useful for gaps)
           --             read from file (mp3)
           --             read from a file (MIDI)
           --       [ ] basic operations:
           --             stretching a sound in time
           --             reverse a sound
           --             combine 2 sound with each other (back to back)
           --             combine 2 sound with each other (in parallel)
           --             play a MIDI using a particular sound font
           -- [ ] and videos
           --       [ ] basic values
           --             read from YouTube
           --             read from DAT
           --             read from Phone video library
           --       [ ] basic operations

type alias Formula = EExpr


type alias CellAddress = ( Int, Int )

row : CellAddress -> Int
row = Tuple.first

column : CellAddress -> Int
column = Tuple.second 

type alias CellMeta =
    String

type alias CometKey = String

type alias Cell =
    { value : Formula
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
imageCell   addr src = { emptyCell | addr = addr, value = EImage src }
graphCell   addr g   = { emptyCell | addr = addr, value = EGraph emptyGraph }

formulaCell addr formula args = { emptyCell | addr = addr, value = EApp formula args }
cometCell   addr endpoint     = { emptyCell | addr = addr, value = EComet endpoint }

youTubeCell addr yt_id yt_start yt_end =
  { emptyCell | addr = addr
              , value = EYouTube { video_id = yt_id, start = yt_start, end = yt_end }
  }

isStringCell cell = case cell.value of
    ESLit _ -> True
    _ -> False

emptySpreadsheet = Spreadsheet [] (IdleMode (0, 0)) [] [] (millisToPosix 0) [] (Dict.fromList [])

type Mode
    = IdleMode CellAddress
    | EditMode CellAddress
    | FileDropMode Mode CellAddress -- One is the cell that the drop is hapening in, the other is the mode we switch back to when drag is over
    | VertexDemoMode
    | EdgeDemoMode1
    | EdgeDemoMode2 VertexAndPerhapsCells
    | RegisterFlushMode Formula


---- is this necessary?
--toString a = case a of
--    IdleMode addr -> "IdleMode " ++ (Debug.toString addr)
--    EditMode addr -> "EditMode" ++ (Debug.toString addr)
--    VertexDemoMode -> "Vertex Demonstration Mode"
--    EdgeDemoMode1 -> "Edge Demonstration Mode (1/2)"
--    EdgeDemoMode2  cellVertex -> "Edge Demonstration Mode (2/2) " ++ (Debug.toString cellVertex)
--    RegisterFlushMode _ -> "Buffer flush mode"
--    --_        -> "Some other mode"


type alias Database = List Cell

type alias Spreadsheet =
    { database : Database
    --, selectionRange : Maybe ( CellAddress, CellAddress )
    , mode : Mode
    , demoVertices : TVertexDemo
    , demoEdges : TEdgeDemo
    , currentTime : Posix
    , previews : List String
    , cometStorage : Dict.Dict CometKey EExpr
    }

