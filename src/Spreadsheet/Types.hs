{-# LANGUAGE DuplicateRecordFields #-}

module Spreadsheet.Types where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char (string)

import Data.Aeson
import qualified Data.Text as T

import Data.Tuple
import qualified Data.Map
-- import Graph (Graph, nodes)
-- import Time (Posix, millisToPosix)
-- import List (any)

import Data.Graph.Inductive.PatriciaTree (Gr)

type List a = [a]
-- type Graph a b = ([a], [b])

data Demonstrations = Demonstrations { vertexDemos :: [VertexAndPerhapsCells], edgeDemos :: TEdgeDemo }
-- data Situation = Situation { database :: Database, demos :: Demonstrations}

emptyDemonstration = Demonstrations { vertexDemos = [], edgeDemos = [] }

infix 7 |>

(|>) a b = b a

-- type Wrangler = (Database -> G)
type Score = Int

type VertexAndPerhapsCells = (EExpr, List Cell)
type EdgeLabel = (Maybe EExpr)
-- type G = Graph (VertexAndPerhapsCells) EdgeLabel

type SuperEdge = (VertexAndPerhapsCells, VertexAndPerhapsCells, EdgeLabel)

type TVertexDemo = List VertexAndPerhapsCells


getCellVertex :: CellAddress -> TVertexDemo -> List VertexAndPerhapsCells
getCellVertex givenAddr demoOfVertices = 
     demoOfVertices
  |> filter (\(_, cells) -> map (\x -> addr x == givenAddr) cells |> any id)

addrInVertexDemo addr demoOfVertices =
  (getCellVertex addr demoOfVertices |> length) > 0

removeAddrFromDemoVertices :: CellAddress -> TVertexDemo -> TVertexDemo
removeAddrFromDemoVertices givenAddr demoOfVertices = 
     demoOfVertices
  |> filter
      (\(_, cells) ->
        map (\x -> addr x == givenAddr) cells |> any not)

type TEdgeDemo   = List SuperEdge

type EFunctor = String
-- These were ported from Elm

data EExpr = EApp EFunctor [EExpr] -- CellFormula, I guess..
           | EILit Int -- CellInt 
           | ESLit String --CellString
           | ECellRef CellAddress 
           | EBot -- CellEmpty
           
        --    | ECellGraph (Graph Cell ())
        --    | EGraphGunrock Gr
           -- ^^^ A super fancy type
           -- that allow you to jump from one cell to another
           -- if cells are linked with respect to a graph (normally graph inside the cellUnderView)

        --    | EGraph (Graph String ()) -- Good old "CellGraph" (<3<3<3)
        --    | EGraphFromDemo TVertexDemo TEdgeDemo


        | EGraphFGL (Gr String Int)

        | EError String
           | EHref String -- CellHref

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
           -- [ ] one frame images
           --       [ ] basic values:
           --             read from file
           --       [ ] basic operations:
           --             whatever "convert" can do (imageMagik)
           -- [ ] animated images (gif images)
           --       [ ] basic values:
           --             read from file (or URL)
           --       [ ] basic operations
           --             things Duke mentioned in his system (w.r.t. combining gif images)  
           -- [ ] and videos
           --       [ ] basic values
           --             read from YouTube
           --             read from DAT
           --             read from Phone video library
           --       [ ] basic operations
           deriving (Show, Eq)

type Formula = EExpr


type CellAddress = ( Int, Int )

row :: CellAddress -> Int
row = fst

column :: CellAddress -> Int
column = snd 

type CellMeta =
    String


data Cell = Cell
    { value :: Formula
    , buffer :: String
    , addr :: CellAddress
    -- Each time you edit a cell, you are modifying the "buffer". 
    -- Once you press enter the buffer will be parsed, and we replace the "value" attribute with
    -- the result of the parser
    --
    -- the logic for the evaluator can be traced in the code that renders the cells to HTML
    , meta  :: Maybe CellMeta
    } deriving (Show, Eq)

-- emptyGraph = Graph.fromNodesAndEdges [] []
emptyGraph = ([], [])
emptyCell = Cell EBot "" (0, 0) Nothing
stringCell  addr str = emptyCell { addr = addr, value = ESLit str }
intCell     addr i   = emptyCell { addr = addr, value = EILit i }
-- graphCell   addr g   = emptyCell { addr = addr, value = EGraph emptyGraph }
formulaCell addr formula args = emptyCell {  addr = addr, value = EApp formula args }

isStringCell cell = case value cell of
    ESLit _ -> True
    _ -> False

-- emptySpreadsheet = Spreadsheet [] (IdleMode (0, 0)) [] [] (millisToPosix 0)
emptySpreadsheet = Spreadsheet [] (IdleMode (0, 0)) [] []


data Mode
    = IdleMode CellAddress
    | EditMode CellAddress
    | VertexDemoMode
    | EdgeDemoMode1
    | EdgeDemoMode2 VertexAndPerhapsCells


-- is this necessary?
toString a = case a of
    IdleMode addr -> "IdleMode " ++ (show addr)
    EditMode addr -> "EditMode" ++ (show addr)
    VertexDemoMode -> "Vertex Demonstration Mode"
    EdgeDemoMode1 -> "Edge Demonstration Mode (1/2)"
    EdgeDemoMode2  cellVertex -> "Edge Demonstration Mode (2/2) " ++ (show cellVertex)
    --_        -> "Some other mode"


type Database = List Cell

data Spreadsheet = Spreadsheet
    { database :: Database
    --, selectionRange : Maybe ( CellAddress, CellAddress )
    , mode :: Mode
    , demoVertices :: TVertexDemo
    , demoEdges :: TEdgeDemo
    -- , currentTime :: Posix
    }



-- instance ToJSON CellAddress
-- instance FromJSON CellAddress


addrToExcelStyle (rho, kappa) = 
    let columnString = [['A'..] !! kappa] in
    mconcat [columnString, show (rho+1)]


data CometValue = CometAddr CellAddress
                | CometString CellAddress String

instance ToJSON CometValue where 
  toJSON (CometAddr addr) =
    object
      [ (T.pack "value") .= (show addr :: String)
      , (T.pack "valueType") .= (T.pack "ESLit")
      , (T.pack "cometKey") .= (T.pack $ addrToExcelStyle addr)
      ]

  toJSON (CometString addr str) = 
    object
      [ (T.pack "value") .= str
      , (T.pack "valueType") .= (T.pack "ESLit")
      , (T.pack "cometKey") .= (T.pack $ addrToExcelStyle addr)
      ]