module Minicell.Types where

import qualified Data.Map.Strict as Map

type Formula = String

type MRow = Int
type MColumn = Int  
data MColor = MRed | MGreen | MBlue


data Addr = Addr { row :: MRow, column :: MColumn } deriving (Eq, Show, Ord)

type MKnowledgeBase = Map.Map Addr MCell

data MCell = MCell { address :: Addr, cellAddress :: Addr, cellValue :: CellValue }
data MVertex = MVertex {color :: MColor, vertexAddress :: Addr, vertexLabel :: CellValue }
data MEdge = MEdge { from :: MVertex, to :: MVertex, edgeLabel :: CellValue }

data CellValue
  = CellInt Int                         -- e.g. 42
  -- | CellGraph (Graph String ())      -- e.g. G = <V, E>
  | CellString String                   -- e.g. "Hello World!"
  | CellFormula Formula                 -- e.g. =Dijkstra(G1, "Davis", "Berkeley")
  | CellEmpty                           -- e.g. ()
  | CellHref String                     -- e.g. http://cs.tufts.edu/~nr/...
  -- | HCellList CellValue [CellValue]     -- e.g. Shouldn't this be "HCellList (List CellValue)" instead?
  | CellHole String                     -- i.e. x@??
  -- | CellKB MKnowledgeBase
  deriving (Show, Eq)