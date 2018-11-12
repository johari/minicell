module Examples.TopoSort exposing (..)

import Graph exposing (..)

import Spreadsheet.Types exposing (..)


vee = Graph.fromNodesAndEdges [ Node 0 (ESLit "center"), Node 1 (ESLit "left"), Node 2 (ESLit "right") ] [ Edge 0 1 () , Edge 0 2 () ]

dressUp =
    let
        nodes =
            [ Node 0     (ESLit "Socks")
            , Node 1     (ESLit "XYZ")
            , Node 2     (ESLit "Pants")
            , Node 3     (ESLit "Shoes")
            , Node 4     (ESLit "XYZ")
            , Node 5     (ESLit "Shirt")
            , Node 6     (ESLit "Belt")
            , Node 7     (ESLit "Tie")
            , Node 8     (ESLit "XYZ")
            , Node 9     (ESLit "excel")
            , Node 10    (ESLit  "research")
            , Node 11    (ESLit  "jobs")
            ]

        e from to =
            Edge from to ()

        edges =
            [ e 0 3 -- socks before shoes
            , e 1 2 -- undershorts before pants
            , e 1 3 -- undershorts before shoes
            , e 2 3 -- pants before shoes
            , e 2 6 -- pants before belt
            , e 5 6 -- shirt before belt
            , e 5 7 -- shirt before tie
            , e 6 8 -- belt before jacket
            , e 7 8 -- tie before jacket
            , e 9 10
            , e 9 11
            ]
    in
    Graph.fromNodesAndEdges nodes edges



-- iWantToWearShoes : List String


iWantToWearShoes model =
    Graph.guidedDfs
        Graph.alongIncomingEdges
        -- which edges to follow
        (Graph.onDiscovery
            (\ctx list ->
                -- append node labels on discovery
                ctx.node.label :: list
            )
        )
        [ 3

        {- "Shoes" NodeId -}
        ]
        -- start with the node labelled "Shoes"
        []
        -- accumulate starting with the empty list
        dressUp
        -- traverse our dressUp graph from above
        |> Tuple.first
