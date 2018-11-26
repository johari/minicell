module Spreadsheet.Wrangling.AdjacencyMatrix exposing (..)

import List.Extra exposing (find)
import Maybe exposing (andThen)

import Graph 

import Spreadsheet.Wrangling.Types exposing (..)
import Spreadsheet.Types exposing (..)

wranglers database = [
     (\_ -> (canonicalMatrixWrangler database))
     ]

--[ _ A B ]
--[ A _ y ]
--[ B _ _ ]

--[ [] [ vertex(A) ] [ vertex(B) ] ]
--[ [ vertex(A) ] [] [ edge(A, B, y) ]
--[ [ vertex(B) ] _ _ ]

-- The synthesis problem is this:

-- The search space of possible programs is this:
-- Map each cell of the spreadsheet to a [Wrangling function].

-- The graph extraction part is a 2 step process.
-- The mechanics of each step is the same 
-- (each cell contributes a set of facts, then we aggregate all the facts)

-- [Step 1]: Figure out what each cell contribute to [facts about vertices]

-- We will assemble this function on
-- every cell of the table that we want to extract a graph from.
--
-- This "decider", when applied uniformly on an adjacency matrix
-- will extract the cells from the "header row" and "header column"
-- and will suggest them as (in this case, the only) candidate for the set of vertices.

kernelVertexWrangling1 : TVertexDemo -> Cell -> List VertexAndPerhapsCells
kernelVertexWrangling1 _ cell =
    case cell.addr of
        (0, 0)     -> predThenSingleVertex (always False) cell -- Warning: it's important for this case to always be on top.
        (0, kappa) -> predThenSingleVertex (always True)  cell -- kappa should be non-zero
        (rho, 0)   -> predThenSingleVertex (always True)  cell -- rho should be non-zero
        _          -> predThenSingleVertex (always False) cell


-- [Step 2]: Figure out what each cell contribute to [facts about edges]

-- Here, it's the opposite of [Step 1].
-- 
-- This time, the cells that inside the table are going to be the main decision makers.
--
-- If the cell inside the table was empty, we don't get any new edges.
-- This simply suggests that the corresponding vertices are not connected.
--
-- The non-empty case is more interesting.
--
-- Each non-empty cell will lookup 2 dear vertices
-- (extracted in [step 1] from header row/column):
--
--      (1) the vertex in header row
--      (2) the vertex in header column
--
-- As an output, it suggests that an edge between (1) and (2) exists,
-- and the capacity of that edge is its own value.
--
--                                               What a cooperation!! 🙏🏾
--                                                                    ^^^^
--                                                         "two people hi-5ing" emoji

kernelEdgeWrangling1 _ vertexList cell =
    case cell.addr of
        (0, 0)          -> [ ]
        (0, kappa)      -> [ ]
        (rho, 0)        -> [ ]
        (rho, kappa)    -> 
            (
                (find (cellAddressIs (rho, 0)) vertexList)   |> andThen (\vertexInHeaderRow ->
                (find (cellAddressIs (0, kappa)) vertexList) |> andThen (\vertexInHeaderColumn ->
                (vertexInHeaderColumn, vertexInHeaderRow, (Just cell.value)) |> Just
            ))) |> (\x -> case x of
                            Nothing -> []
                            Just a  -> [ a ]
                 )

canonicalMatrixWrangler : Database -> G
canonicalMatrixWrangler database = matrixWranglerWithVertexExample database []

matrixWranglerWithVertexExample : Database -> List VertexAndPerhapsCells -> G
matrixWranglerWithVertexExample database vertexExamples =
    -- FIXME: use vertexExamples!!
    -- Let's ignore symmetry and pretend there's only one way for adjacency matrix for now
    --
    let vertexSet = List.concatMap (kernelVertexWrangling1 []) database
    in
        (List.concatMap (kernelEdgeWrangling1 [] vertexSet) database) |> constructFullGraph vertexSet