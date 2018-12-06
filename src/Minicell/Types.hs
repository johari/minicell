{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Minicell.Types where

import GHC.Generics
import Data.Aeson

import Data.Text as T

import qualified Data.Map.Strict as Map

type Formula = String

type MRow = Int
type MColumn = Int  
data MColor = MRed | MGreen | MBlue


data Addr = Addr { row :: MRow, column :: MColumn } deriving (Eq, Show, Ord, Generic)

instance ToJSON Addr
instance FromJSON Addr

data CometValue = CometAddr Addr

instance ToJSON CometValue where
  toJSON (CometAddr addr) =
    object
      [ (T.pack "value") .= (42 :: Int)
      , (T.pack "valueType") .= (T.pack "EILit")
      , (T.pack "cometKey") .= (T.pack "A1") -- (T.pack $ addrToExcelStyle addr)
      ]

type MKnowledgeBase = Map.Map Addr MCell

data MCell = MCell { address :: Addr, cellAddress :: Addr, cellValue :: CellValue }
data MVertex = MVertex {color :: MColor, vertexAddress :: Addr, vertexLabel :: CellValue }
data MEdge = MEdge { from :: MVertex, to :: MVertex, edgeLabel :: CellValue }


                  
-- | CellHref String                     -- e.g. http://cs.tufts.edu/~nr/...
-- | HCellList CellValue [CellValue]     -- e.g. Shouldn't this be "HCellList (List CellValue)" instead?
-- | CellGraph (Graph String ())      -- e.g. G = <V, E>
-- | CellKB MKnowledgeBase


data CellValue = CellInt Int                         -- e.g. 42
                  | CellString String                   -- e.g. "Hello World!"
                  | CellFormula Formula                 -- e.g. =Dijkstra(G1, "Davis", "Berkeley")
                  | CellEmpty                           -- e.g. ()
                  | CellHole String                     -- i.e. x@??
                  deriving (Show, Eq)
