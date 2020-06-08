{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Spreadsheet.Types where

import Data.Char (ord, toLower, toUpper)

--

import GHC.Generics
import Codec.Serialise
import Codec.Serialise.Encoding
import Codec.CBOR

----


import Text.Read

import Debug.Trace

-- Diagrams stuff

import Diagrams.Prelude (Diagram, circle)
import Diagrams.Backend.SVG

-- Graph stuff

import Data.Graph.Inductive.Example

import Data.Graph.Inductive.Dot

import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.Graph
-- Text and JSON stuff

import Data.Aeson
import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import Text.Parsec.Char (string)

-- Haskell stuff

import Data.Tuple
import Data.Maybe
import qualified Data.Map

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

-- <hack>

data XDiagram = XDiagram (Diagram B)

instance Eq XDiagram where
  (==) _ _ = True

instance Show XDiagram where
  show _ = ""

instance Read XDiagram where
  readsPrec _ = const []

-- </hack>

data EExpr = EApp EFunctor [EExpr] -- CellFormula, I guess..
           | EILit Int -- CellInt
           | ESLit String --CellString
           | EHTML String --CellString

           | EJumpLink String String

           | EList [EExpr] -- Should we allow [ESLit, EIlit]? Perhaps not :(
           | ETuple2 (EExpr, EExpr)
           | ETuple3 (EExpr, EExpr, EExpr)

           | ECellRef CellAddress
           | ECellRange CellAddress CellAddress

           | EVar String

           | EBot -- CellEmpty

           | EEmpty

           | EAudio String

           | EYouTube { yt_id :: Int, yt_start :: Maybe Double, yt_end :: Maybe Double }

           | EImage String
           | EVideo String

        --    | ECellGraph (Graph Cell ())
        --    | EGraphGunrock Gr
           -- ^^^ A super fancy type
           -- that allow you to jump from one cell to another
           -- if cells are linked with respect to a graph (normally graph inside the cellUnderView)

        --    | EGraph (Graph String ()) -- Good old "CellGraph" (<3<3<3)
        --    | EGraphFromDemo TVertexDemo TEdgeDemo


        | EGraphFGL (Gr String Int)
        | EGraphFGLAttr (Gr String String)
        | EGraphPtr String
        | EDataFrame [String] [[String]]

        -- | EGraphFGLX (Gr EExpr EExpr)

        | EDiag XDiagram
        | ENotImplemented

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
           deriving (Show, Eq, Read, Generic)


normalizeOp expr =
  case expr of
    EApp op args -> EApp (map toUpper op) args
    _ -> expr

type Formula = EExpr

instance Serialise XDiagram where
  encode _ = encodeInt 1
  decode = do
   return $ XDiagram $ circle 1

instance Serialise (Gr String Int)
instance Serialise (Gr String String)

instance Serialise EExpr

eexprToHtml cellValue = do
    case cellValue of
        ESLit s -> return s
        _ -> return $ "do not know how to convert " ++ (show cellValue) ++ " to html"


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
    , formulaStr :: String
    -- Each time you edit a cell, you are modifying the "buffer".
    -- Once you press enter the buffer will be parsed, and we replace the "value" attribute with
    -- the result of the parser
    --
    -- the logic for the evaluator can be traced in the code that renders the cells to HTML
    , meta  :: Maybe CellMeta
    } deriving (Show, Eq, Read, Generic)

instance Serialise Cell

-- emptyGraph = Graph.fromNodesAndEdges [] []
emptyGraph = ([], [])
emptyCell = Cell { addr = (0, 0), value = EBot, buffer =  "", meta = Nothing, formulaStr = "" }

jumpLinkCell  addr label src = emptyCell { addr = addr, value = EJumpLink label src }

stringCell  addr str = emptyCell { addr = addr, value = ESLit str }
intCell     addr i   = emptyCell { addr = addr, value = EILit i }
graphCell   addr g   = emptyCell { addr = addr, value = EGraphFGL g }
formulaCell addr formula args = emptyCell {  addr = addr, value = EApp formula args }

isStringCell cell = case value cell of
    ESLit _ -> True
    _ -> False

-- emptySpreadsheet = Spreadsheet [] (IdleMode (0, 0)) [] [] (millisToPosix 0)
emptySpreadsheet = Spreadsheet [] (IdleMode (0, 0)) [] [] (Data.Map.fromList [])




spreadsheetWithOneGraph = Spreadsheet [ graphCell (0, 0) vor ] (IdleMode (0, 0)) [] []

data Mode
    = IdleMode CellAddress
    | EditMode CellAddress
    | VertexDemoMode
    | EdgeDemoMode1
    | EdgeDemoMode2 VertexAndPerhapsCells deriving (Show, Read, Eq, Generic)

instance Serialise Mode

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
    , cache :: Data.Map.Map String EExpr
    } deriving (Show, Read, Eq, Generic)

instance Serialise Spreadsheet

refs :: EExpr -> [CellAddress]
refs eexpr = case eexpr of
  ECellRef addr -> [addr]
  EApp _ args -> concat (refs <$> args)
  _ -> []

dependencyGraph :: Database -> Gr CellAddress ()
dependencyGraph db = trace (show $ edges) (mkGraph vertices edges)
  where
    (vertices, nm) = mkNodes new [ addr c | c <- db ]
    edges = fromMaybe [] $ mkEdges nm [ (v2, v1, ()) | c <- db, let v1 = addr c, v2 <- refs (value c) ]
  -- vertices: non-empty cells
  -- edges: if a cell addresses another cell (ECellRef addr), then the cell points to addr



-- instance ToJSON CellAddress
-- instance FromJSON CellAddress


addrRhoToExcelStyle rho = show (rho + 1)
addrKappaToExcelStyle kappa = [['A'..] !! kappa]

addrToExcelStyle (rho, kappa) =
    let columnString = addrKappaToExcelStyle kappa in
    mconcat [columnString, addrRhoToExcelStyle rho]


data CometValue = CometAddr String CellAddress
                | CometSLit String CellAddress String
                | CometHTML String CellAddress String
                | CometILit String CellAddress Int
                | CometImage String CellAddress String
                | CometVideo String CellAddress String
                | CometEmpty String CellAddress
                | CometJumpLink String CellAddress String String

instance ToJSON EExpr where
  toJSON s = object [ (T.pack "formula") .= (show s :: String) ]

instance ToJSON CometValue where
  toJSON (CometEmpty fx addr) =
    object
      [ (T.pack "valueType") .= (T.pack "EEmpty")
      , (T.pack "cometKey") .= (T.pack $ addrToExcelStyle addr)
      , (T.pack "formula") .= (T.pack $ fx)
      ]


  toJSON (CometAddr fx addr) =
    object
      [ (T.pack "value") .= (show addr :: String)
      , (T.pack "valueType") .= (T.pack "ESLit")
      , (T.pack "cometKey") .= (T.pack $ addrToExcelStyle addr)
      , (T.pack "formula") .= (T.pack $ fx)
      ]

  toJSON (CometJumpLink fx addr label src) =
    object
      [ (T.pack "value") .= [label, src]
      , (T.pack "valueType") .= (T.pack "EJumpLink")
      , (T.pack "cometKey") .= (T.pack $ addrToExcelStyle addr)
      , (T.pack "formula") .= (T.pack $ fx)
      ]

  toJSON (CometHTML fx addr str) =
    object
      [ (T.pack "value") .= str
      , (T.pack "valueType") .= (T.pack "EHTML")
      , (T.pack "cometKey") .= (T.pack $ addrToExcelStyle addr)
      , (T.pack "formula") .= (T.pack $ fx)
      ]

  toJSON (CometSLit fx addr str) =
    object
      [ (T.pack "value") .= str
      , (T.pack "valueType") .= (T.pack "ESLit")
      , (T.pack "cometKey") .= (T.pack $ addrToExcelStyle addr)
      , (T.pack "formula") .= (T.pack $ fx)
      ]

  toJSON (CometILit fx addr i) =
    object
      [ (T.pack "value") .= i
      , (T.pack "valueType") .= (T.pack "EILit")
      , (T.pack "cometKey") .= (T.pack $ addrToExcelStyle addr)
      , (T.pack "formula") .= (T.pack $ fx)
      ]


  toJSON (CometImage fx addr src) =
    object
      [ (T.pack "value") .= src
      , (T.pack "valueType") .= (T.pack "EImage")
      , (T.pack "cometKey") .= (T.pack $ addrToExcelStyle addr)
      , (T.pack "formula") .= (T.pack $ fx)
      ]

  toJSON (CometVideo fx addr src) =
    object
      [ (T.pack "value") .= src
      , (T.pack "valueType") .= (T.pack "EVideo")
      , (T.pack "cometKey") .= (T.pack $ addrToExcelStyle addr)
      , (T.pack "formula") .= (T.pack $ fx)
      ]
