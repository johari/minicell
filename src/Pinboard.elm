module Pinboard exposing (Bookmark, Recommendation, decoder, dressUp, getBookmarks, getRecommendations, iWantToWearShoes, pinboardGraph, viewBookmarkTable, viewRecommendationsText, viewSingleBookmark)

import Dict
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Graph.DOT as DOT exposing (..)
import Html exposing (Html, b, br, button, code, div, h1, hr, input, li, ol, strong, text, ul)
import Http
import Json.Decode as D
import List exposing (concat, map)
import List.Extra exposing (intercalate, unique, zip)
import Maybe exposing (withDefault)
import String exposing (join, split)
import Stylize exposing (..)
import Tuple exposing (first, second)


type alias Recommendation =
    { other_tag : String
    , correlation : Float
    }


decoderRecommendation : D.Decoder Recommendation
decoderRecommendation =
    D.map2 Recommendation
        (D.field "other_tag" D.string)
        (D.field "score" D.float)


getRecommendations : String -> Cmd (Result Http.Error (List Recommendation))
getRecommendations query =
    Http.send (\x -> x) (Http.get ("http://localhost:4567/correlate/tag/" ++ query) (D.list decoderRecommendation))



-- node labels are strings, edge labels are empty
-- dressUp : Graph String ()


type alias Bookmark =
    { href : String
    , description : String
    , tags : List String
    , time : String
    }


decoder : D.Decoder Bookmark
decoder =
    D.map4 Bookmark
        (D.field "href" D.string)
        (D.field "description" D.string)
        (D.field "tags" (D.list D.string))
        (D.field "time" D.string)


getBookmarks : String -> Cmd (Result Http.Error (List Bookmark))
getBookmarks query =
    Http.send (\x -> x) (Http.get ("http://localhost:4567/api/" ++ query) (D.list decoder))


pinboardGraph : List Bookmark -> Graph String ()
pinboardGraph bookmarks =
    let
        hostnamesWithTags =
            map (\x -> ( x.href, x.tags )) bookmarks

        allTags =
            map second hostnamesWithTags |> concat |> unique

        tagDict =
            Dict.fromList (zip allTags (List.range 0 100))

        allHostnames =
            map first hostnamesWithTags |> map extractHostName |> unique

        hnDict =
            Dict.fromList (zip allHostnames (List.range 200 300))

        edges =
            map
                (\x ->
                    let
                        hnKey =
                            Dict.get (extractHostName x.href) hnDict |> withDefault 0

                        tagKeys =
                            map (\tag -> Dict.get tag tagDict) x.tags |> map (withDefault 0)
                    in
                    zip (List.repeat (List.length tagKeys) hnKey) tagKeys
                )
                bookmarks

        allNodes =
            map (\( y, x ) -> Node x y) (Dict.toList tagDict ++ Dict.toList hnDict)
    in
    Graph.fromNodesAndEdges allNodes (concat edges |> map (\( x, y ) -> Edge y x ()))


viewRecommendationsText rs =
    case rs of
        [] ->
            Html.span [] [ text "no recommendations" ]

        _ ->
            Html.span []
                ([ text "See also: " ]
                    ++ (List.map (\x -> [ x.other_tag ]) rs
                            |> intercalate [ ", " ]
                            |> List.map text
                            |> List.map (\x -> strong [] [ x ])
                            |> List.take 10
                       )
                )


viewBookmarkTable urls description_query =
    Html.table [] (List.map (viewSingleBookmark description_query) urls)


viewSingleBookmark description_query b =
    Html.tr []
        [ text ""

        -- , Html.td [] [ text b.href ]
        , Html.td [] [ stylizeHostname b.href ]
        , Html.td [] [ vgrep b.description description_query "label_2" ]
        , Html.td [] [ Html.b [] [ vgrep (join " " b.tags) "tasks" "label_1" ] ]

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
