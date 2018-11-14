module Spreadsheet.Wrangling.Types exposing (..)

import Maybe.Extra
import Maybe exposing (andThen)

import Spreadsheet.Types exposing (..)
import Graph exposing (..)

import List
import List.Extra exposing (elemIndex)

import Tuple

type WranglingMode = AdjacencyMatrix | ListOfVertexTuples

predThenSingleVertex : (Cell -> Bool) -> Cell -> List VertexAndPerhapsCells -- List of length 1, hence "Single" in the name
predThenSingleVertex pred cell = if pred cell then [ (cell.value, [cell]) ] else [ ]

cellAddressIs : CellAddress -> VertexAndPerhapsCells -> Bool
cellAddressIs addr (_, cellList) = List.any (\cell -> cell.addr == addr) cellList

sameRow : Cell -> Cell -> Bool
sameRow cell1 cell2 = row cell1.addr == row cell2.addr

sameRowButDifferentColumn : Cell -> Cell -> Bool
sameRowButDifferentColumn cell1 cell2 = (row cell1.addr == row cell2.addr) && (column cell1.addr /= column cell2.addr)

-- This is an example of a [wrangling kernel].
-- In particular, this is a [vertex wrangling kernel]

--kernelVertexWrangling : TVertexDemo -> Cell -> List VertexAndPerhapsCells

-- This is an [edge wrangling] kernel

--kernelEdgeWrangling : TEdgeDemo -> List VertexAndPerhapsCells -> Cell -> List SuperEdge

constructFullGraph : List VertexAndPerhapsCells -> List SuperEdge -> G 
constructFullGraph vertices edges = 
    let enumeratedVertices =
            List.indexedMap Tuple.pair vertices 

        vertices2 =
            enumeratedVertices |> List.map (\(x, y) -> Node x y)

        edges2 =
            edges |> List.map (\(v1, v2, c) -> elemIndex v1 vertices |> andThen (\i1 ->
                                               elemIndex v2 vertices |> andThen (\i2 ->
                                               Just (Edge i1 i2 c)
                  ))) |> Maybe.Extra.values
    in
        Graph.fromNodesAndEdges vertices2 edges2
