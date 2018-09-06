module Pinboard exposing (Bookmark, decoder, dressUp, getBookmarks, iWantToWearShoes, urlsToGraph)

import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Graph.DOT as DOT exposing (..)
import Html exposing (Html, b, br, button, code, div, h1, hr, input, li, ol, strong, text, ul)
import Http
import Json.Decode as D
import Stylize exposing (..)



-- node labels are strings, edge labels are empty
-- dressUp : Graph String ()


type alias Bookmark =
    { href : String
    , description : String
    , tags : String
    , time : String
    }


decoder : D.Decoder Bookmark
decoder =
    D.map4 Bookmark
        (D.field "href" D.string)
        (D.field "description" D.string)
        (D.field "tags" D.string)
        (D.field "time" D.string)


getBookmarks : Cmd (Result Http.Error (List Bookmark))
getBookmarks =
    Http.send (\x -> x) (Http.get "../examples/pinboard-jobs.json" (D.list decoder))


urlsToGraph : List Bookmark -> Graph String ()
urlsToGraph urls =
    Graph.fromNodesAndEdges [] []


viewBookmarkTable urls description_query =
    Html.table [] (List.map (viewSingleBookmark description_query) urls)


viewSingleBookmark description_query b =
    Html.tr []
        [ text ""

        -- , Html.td [] [ text b.href ]
        , Html.td [] [ stylizeHostname b.href ]
        , Html.td [] [ vgrep b.description description_query "label_2" ]
        , Html.td [] [ Html.b [] [ vgrep b.tags "tasks" "label_1" ] ]

        -- , Html.td [] [ text (extractHostName b.href) ]
        , Html.td [] [ vgrep b.time "2018-07-" "label_time" ]
        ]


dressUp model =
    let
        nodes =
            [ Node 0 "Socks"
            , Node 1 model.description_query
            , Node 2 "Pants"
            , Node 3 "Shoes"
            , Node 4 model.description_query
            , Node 5 "Shirt"
            , Node 6 "Belt"
            , Node 7 "Tie"
            , Node 8 model.description_query
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
        (dressUp model)
        -- traverse our dressUp graph from above
        |> Tuple.first



-- ignores the untraversed rest of the graph
-- iWantToWearShoes == ["Pants", "Undershorts", "Socks", "Shoes"]
-- CAUSE OF DEATH                 .
-- toGiphyUrl : String -> String
-- toGiphyUrl topic =
--     Url.crossOrigin "https://api.giphy.com"
--         [ "v1", "gifs", "random" ]
--         [ Url.string "api_key" "dc6zaTOxFJmzC"
--         , Url.string "tag" topic
--         ]
