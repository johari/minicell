module Spreadsheet.Wrangling.Ranking exposing (..)

import Spreadsheet.Types exposing (..)

import Set
import List
import Graph exposing (nodes, edges)

emptyWrangler = (always emptyGraph)

pickBestWithRespectTo : Situation -> List Wrangler -> Wrangler
pickBestWithRespectTo situation listOfWranglers =
    let
        metric = (\wrangler -> (demonstrationFaithfulnessScore situation) wrangler)
    in
        List.sortBy metric listOfWranglers |> List.head |> Maybe.withDefault emptyWrangler


--demonstrationFaithfulnessScore : Situation -> Wrangler -> Score
demonstrationFaithfulnessScore situation wrangler =
    let graph = wrangler (situation.database)
    in
        (similarityToVertexDemos situation.demos.vertexDemos (nodes graph)) +
        (similarityToEdgeDemos situation.demos.edgeDemos graph)

--vertexToCellVertex : Graph.NodeId -> G -> VertexAndPerhapsCells
vertexToCellVertex nodeId graph =
    case (Graph.get nodeId graph) of
        Just context -> context.node.label
        Nothing -> (EBot, [])

-- Implementing these functions was a bit boring (or intimidating?)
-- I was not in a good mood when I implemented them. 🤕
-- Be careful with the rest of this source file.

-- I feel like my abstractions are only a thin layer over each other
-- and I'm just doing the manual labor of converting between them,
-- whereas a DSL could easily express "my intent"
-- (find "the best wrangler" with respect to demonstrations)
-- in a concise and clear way

-- Although the lines below implement my intent (hopefully),
-- they do not "reflect it".

-- Perhaps I need to find a better way to express my intent.
-- I need a collection of clear expressions that reflect my intent.

--convertGraphEdgeToSuperEdge : G -> Graph.Edge EdgeLabel -> SuperEdge
convertGraphEdgeToSuperEdge graph edge =
    (vertexToCellVertex (edge.from) graph, vertexToCellVertex (edge.to) graph, edge.label)

--similarityToEdgeDemos : TEdgeDemo -> G -> Int
--similarityToEdgeDemos edgeDemos graph =
--    let setA = Set.fromList edgeDemos
--        setB = Set.fromList (edges graph |> List.map (convertGraphEdgeToSuperEdge graph))
--    in
--        Set.toList (Set.intersect setA setB) |> List.length

similarityToEdgeDemos _ _ = 0

-- [ugh]

-- I got so many type errors here. More than this simple task required.
-- Not so happy about that. Here's an example:

--This argument is:

--    List VertexAndPerhapsCells

--But `fromList` needs the 1st argument to be:

--    List ( EExpr, List Cell )

-- However, I have

--    type alias VertexAndPerhapsCells = (EExpr, List Cell)

-- The type system is overwhelmed by the way I'm trying to go about solving this problem..

-- [/ugh]


-- Oh man.. seems like this is actually a bug in the compiler..
-- https://github.com/elm/compiler/issues/1192
-- :(
-- :(
-- :(

--similarityToVertexDemos : TVertexDemo -> List (Graph.Node VertexAndPerhapsCells) -> Int
--similarityToVertexDemos vertexDemos verticesInGraph =
--    let setA = (vertexDemos) |> Set.fromList
--        setB = (verticesInGraph |> List.map (\v -> v.label)) |> Set.fromList
--    in
--        Set.toList (Set.intersect setA setB) |> List.length

similarityToVertexDemos _ _  = 0