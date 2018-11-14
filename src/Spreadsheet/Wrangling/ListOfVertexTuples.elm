module Spreadsheet.Wrangling.ListOfVertexTuples exposing (..)

import Spreadsheet.Wrangling.Types exposing (..)
import Spreadsheet.Types exposing (..)

--[ url tags ]
--[ "github.com/microsoft/prose" "program-synthesis" ]
--[ "conf.researchr.org/home/pldi-2019" "pldi 2019 deadline" ] -- edge from vertices extracted from column `tags` to vertices extracted form column `url`

--[ [vertex("github.com/microsoft/prose")] [vertex("program-synthesis"), edge("github.com/microsoft/prose", "program-synthesis")]]
--[ [vertex("conf.researchr.org/home/pldi-2019")] [vertex("pldi"), vertex("2019"), vertex("deadline"), edge("conf.researchr.org/home/pldi-2019", "pldi"), edge("conf.researchr.org/home/pldi-2019", "deadline"), edge("conf.researchr.org/home/pldi-2019", "2019")]]




-- However, we are only interested in figuring out a recipe for one row (or one column)
-- and repeat that for an entire region..

-- So in reality, the "search space of programs" is O(size of one record).
-- Even then.. we don't check all the fields..
--- In most cases, we do for only 2 (or at least that's the easiest case).


-- TODO: only try rows (or, non-deterministically, columns) that appear in TVertexDemo (= demos)
kernelVertexWrangling1 : TVertexDemo -> Cell -> List VertexAndPerhapsCells
kernelVertexWrangling1 demos cell =
    case cell.addr of
        (0, 0)     -> predThenSingleVertex (always False) cell -- Warning: it's important for this case to always be on top.
        (0, kappa) -> predThenSingleVertex (always True)  cell -- kappa should be non-zero
        (rho, 0)   -> predThenSingleVertex (always True)  cell -- rho should be non-zero
        _          -> predThenSingleVertex (always False) cell


--predThenEdgeBetweenTwoVertex : (Cell -> Cell -> Bool) -> Proposition -> Proposition -> List Proposition
--predThenEdgeBetweenTwoVertex prop1 prop2 = case (prop1, prop2) of
--    ((FVertex cell1), (FVertex cell2)) ->
--        if pred cel1 cell2
--            [ FEdge cell1 cell2 ]
--        else
--            [ ]
--    _ -> [ ]

--sameRowThenEdge = predThenEdge (sameRowButDifferentColumn)

-- Gather all the vertices together,
-- take a Cartesian product
-- among pairs that are in the same row, connect all together

--edgeWrangler_sameRowManyToMany predForOne assumptions  =
--    assumptions `andThen` \x ->
--    assumptions `andThen` \y ->
--    sameRowThenEdge x y

databaseToCandidateGraphs : Database -> TVertexDemo -> TEdgeDemo -> List G
databaseToCandidateGraphs database mode vertexExamples = []
    --    -- Let's ignore symmetry and pretend there's only one way for adjacency matrix for now
    --    let vertexSet = List.concatMap (kernelVertexWrangling1 []) database
    --    in
    --        (List.concatMap (kernelEdgeWrangling1 [] vertexSet) database) |> constructFullGraph vertexSet
    --_ -> Graph.fromNodesAndEdges [] []

-- among pairs that are in the same row, connect all to a designated sink

--edgeWrangler_sameRowManyToOne predForOne assumptions  =
--    assumptions `andThen` \x ->
--    assumptions `andThen` \y ->
--    sameRowThenEdge x y


-- I can identify 2 important "Templates" for wrangling.
-- One is the adjacency matrix
-- The other one is list of node pairs (each record is one edge)

-- There's a third one that I leave to future work:
-- cellular automata (grids that represent the map, the same style you see in competetive programmming riddles).