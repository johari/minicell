module Pinboard exposing (Bookmark, decoder, getBookmarks, pinboardGraph, viewBookmarkTable, viewSingleBookmark)

import Dict
import Examples.Outline exposing (..)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Graph.DOT as DOT exposing (..)
import Html exposing (Html, b, br, button, code, div, h1, hr, input, li, ol, strong, text, ul)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode as D
import List exposing (concat, map)
import List.Extra exposing (unique, zip)
import Maybe exposing (withDefault)
import String exposing (join, split)
import Stylize exposing (..)
import Tuple exposing (first, second)



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


tableTopRow el =
    Html.tr [ class "top-row" ]
        [ Html.td [] [ text "A" ]
        , Html.td [] [ text "B" ]
        , Html.td [] [ text "C" ]
        , Html.td [] [ text "D" ]
        ]


tableLeftColumn el =
    el


viewBookmarkTable urls description_query =
    let
        el =
            List.map (viewSingleBookmark description_query) urls
    in
    Html.table [ class "spreadsheet" ] ([ tableTopRow el ] ++ tableLeftColumn el)


viewSingleBookmark description_query b =
    Html.tr []
        [ text ""

        -- , Html.td [] [ text b.href ]
        , Html.td []
            [ Html.a [ href b.href ]
                [ stylizeHostname b.href
                ]
            ]
        , Html.td [] [ vgrep b.description description_query "label_2" ]
        , Html.td []
            [ Html.b [] [ vgrep (join " " b.tags) "tasks" "label_1" ]
            ]

        -- , Html.td [] [ text (extractHostName b.href) ]
        , Html.td [] [ vgrep b.time "2018-07-" "label_time" ]
        ]
