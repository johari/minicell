module Examples.TopoSort exposing (dressUp)

import Graph exposing (..)


dressUp =
    let
        nodes =
            [ Node 0 "Socks"
            , Node 1 "XYZ"
            , Node 2 "Pants"
            , Node 3 "Shoes"
            , Node 4 "XYZ"
            , Node 5 "Shirt"
            , Node 6 "Belt"
            , Node 7 "Tie"
            , Node 8 "XYZ"
            , Node 9 "iohk.io"
            , Node 10 "iohk"
            , Node 11 "jobs"
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
