{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings     #-}


module Minicell.Pluck where



import Data.String
import Data.Int
import Data.Maybe

import Data.Map.Strict as Map

import Control.Monad (guard)

import Minicell.Types

import Minicell.Examples.Graphs
import Minicell.Examples.Tables

import Data.SBV
import Data.SBV.Control

import Data.SBV.List ((.!!))
import qualified Data.SBV.List as L



{-
  0. [for now assume matrix representation]
  1. Convert the adjacency matrix into Prolog relations
  2. 
-}

headerRowToSetOfVertices [] = []
headerRowToSetOfVertices (x:xs) =
  case x of
    CellString label -> [label] ++ (headerRowToSetOfVertices xs)
    _ -> headerRowToSetOfVertices xs

pluckVertices table demonstrations = zip ([0..]) $ headerRowToSetOfVertices (table !! 0)

-- tableToMap [[]] = Map.empty
-- tableToMap [["a", "b"], ["c", "d"]] = fromList [((0,0), "a"), ((0, 1), "b") ... ]

{- Must return a list of triples -}
pluckEdges table pluckedVertices demonstrations = citiesGraphLEdges


exampleList = sat $ do
  (a :: SList [Integer]) <- free "a"
  constrain $ a .!! 0 .== [1, 2, 3]
  constrain $ a .!! 1 .== [4, 5, 6, 7]
  constrain $ L.tail (L.tail a) .== [[8, 9, 10], [11, 12, 13]]
  constrain $ L.length a .== 4


{-
  edge('davis', 'berkeley', V) :- cell(0, R, 'davis')
                                 , cell(C, 0, 'berkeley')
                                 , cell(C, R, V).

                                  -}

linkTwoVertexInAdjacencyMatrix :: MKnowledgeBase -> MVertex -> MVertex -> [MEdge]
linkTwoVertexInAdjacencyMatrix kb source destination = do
  cellS <- elems kb
  cellD <- elems kb
  guard $ (cellValue cellS) == (vertexLabel source)
  guard $ (cellValue cellD) == (vertexLabel destination)
  guard $ (column $ vertexAddress source)      == 0
  guard $ (row    $ vertexAddress destination) == 0
  return $ MEdge { from = source
                 , to = destination
                 , edgeLabel = case maybeCell of
                                Nothing -> CellEmpty
                                Just c  -> cellValue c} 
  where
    maybeCell = (flip Map.lookup) kb $ 
      Addr { row    = (row $ vertexAddress source)
           , column = (column $ vertexAddress destination)
           }

extractEdgeFromTable :: [[String]] -> String -> String -> IO SatResult
extractEdgeFromTable table source destination = sat $ do
  rowNum :: SInteger <- exists "rowNum"
  colNum :: SInteger <- exists "colNum"
  sTable :: SList [String] <- free "sTable"

  -- initialize a symbolic table
  -- sequence_ [constrain $ sTable .!! (row) .!! (col) .== (fromString $ table !! row !! col) | row <- [0..(length table)], col <- [0..(length $ table !! 0)]]
  constrain $ sTable .!! 0 .!! 0 .== (fromString $ table !! 0 !! 0)
  constrain $ sTable .!! 0 .!! 1 .== (fromString $ table !! 0 !! 1)
  constrain $ sTable .!! 0 .!! 2 .== (fromString $ table !! 0 !! 2)
  constrain $ sTable .!! 0 .!! 3 .== (fromString $ table !! 0 !! 3)
  
  constrain $ sTable .!! 1 .!! 0 .== (fromString $ table !! 1 !! 0)
  constrain $ sTable .!! 1 .!! 1 .== (fromString $ table !! 1 !! 1)
  constrain $ sTable .!! 1 .!! 2 .== (fromString $ table !! 1 !! 2)
  constrain $ sTable .!! 1 .!! 3 .== (fromString $ table !! 1 !! 3)
  
  constrain $ sTable .!! 2 .!! 0 .== (fromString $ table !! 2 !! 0)
  constrain $ sTable .!! 2 .!! 1 .== (fromString $ table !! 2 !! 1)
  constrain $ sTable .!! 2 .!! 2 .== (fromString $ table !! 2 !! 2)
  constrain $ sTable .!! 2 .!! 3 .== (fromString $ table !! 2 !! 3)
  
  constrain $ sTable .!! 3 .!! 0 .== (fromString $ table !! 3 !! 0)
  constrain $ sTable .!! 3 .!! 1 .== (fromString $ table !! 3 !! 1)
  constrain $ sTable .!! 3 .!! 2 .== (fromString $ table !! 3 !! 2)
  constrain $ sTable .!! 3 .!! 3 .== (fromString $ table !! 3 !! 3)
  
  -- constrain $ sTable .== table 

  constrain $ rowNum .< 5
  constrain $ colNum .< 5
  constrain $ sTable .!! 0 .!! colNum .== (fromString destination)
  constrain $ sTable .!! rowNum .!! 0 .== (fromString source)
