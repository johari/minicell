port module Main exposing (Msg(..), main, update, view)

import Browser
import Examples.Outline exposing (..)
import Graph.DOT as DOT exposing (..)
import Html exposing (Html, b, br, button, code, div, h1, hr, input, li, node, ol, strong, table, td, text, tr, ul)
import Html.Attributes exposing (attribute, class, href, id, placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import List
import List.Extra exposing (intercalate)
import Pinboard as P exposing (..)
import Platform.Cmd exposing (batch, map)
import Spreadsheet
import Stylize exposing (..)
import Svg exposing (svg)
import Svg.Attributes exposing (height, width)
import Task exposing (perform, succeed)


port repaintGraph : String -> Cmd msg


port renderPaperOutline : String -> Cmd msg


type Msg
    = NewJSON (Result Http.Error (List Bookmark))
    | NewRecommendation (Result Http.Error (List Recommendation))
    | URLSearch String
    | SpreadsheetMsg Spreadsheet.Msg


type alias Model =
    { urls : List Bookmark
    , description_query : String
    , recommendations : List Recommendation
    , spreadsheet : Spreadsheet.Spreadsheet
    }


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] "writing" [] Spreadsheet.exampleSpreadsheet
    , batch
        [ map (\x -> NewJSON x) (getBookmarks "writing")
        , map (\x -> NewRecommendation x) (getRecommendations "writing")
        , renderPaperOutline paperOutline
        ]
    )


update msg model =
    case msg of
        SpreadsheetMsg _ ->
            ( model, Cmd.none )

        URLSearch s ->
            let
                newModel =
                    { model | description_query = s }
            in
            ( newModel
            , batch
                [ repaintGraph (output Just (always Nothing) (pinboardGraph newModel.urls))
                , map (\x -> NewJSON x) (getBookmarks newModel.description_query)
                , map (\x -> NewRecommendation x) (getRecommendations newModel.description_query)
                ]
            )

        NewRecommendation result ->
            case result of
                Ok ls ->
                    let
                        newModel =
                            { model | recommendations = ls }
                    in
                    ( newModel, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        NewJSON result ->
            case result of
                Ok ls ->
                    let
                        newModel =
                            { model | urls = ls, spreadsheet = bookmarkTableToSpreadsheet ls model.description_query }
                    in
                    ( newModel, repaintGraph (output Just (always Nothing) (pinboardGraph newModel.urls)) )

                Err _ ->
                    ( { model | urls = [] }, Cmd.none )


view model =
    table [ id "table-main" ]
        [ -- this is related to our recommendation engine
          tr []
            [ td [ attribute "colspan" "2" ]
                [ text "enter your query: "
                , input [ onInput URLSearch, placeholder model.description_query ] []
                , br [] []
                , br [] []
                , viewRecommendationsText model.recommendations
                ]
            ]

        -- this section pertains to rendering the table and then the d3 visualization
        , tr [ id "tr-sheet-and-visualization" ]
            [ td [ id "td-sheet-1" ] [ subView model ]
            , td [ id "td-stdout-1" ] [ svg [ id "svg-d3", width "300", height "300" ] [] ]
            ]

        -- the following section is where the magic happens:
        , tr [] [ Spreadsheet.view model.spreadsheet |> Html.map SpreadsheetMsg ]

        -- misc
        , tr [] [ td [ attribute "colspan" "2" ] [ div [ id "svg-container" ] [] ] ]
        ]


subView model =
    div []
        [ div [] [ viewBookmarkTable model.urls model.description_query ]

        --, br [] []
        --, br [] []
        --, hr [] []
        ]


misc =
    div []
        [ br [] []
        , h1 [] [ text "TODO" ]
        , ol []
            [ li [] [ text "Navigate to cells (goto the cell with the highest in-degree)" ]
            , li [] [ text "Subgraph matching (for prunning and reducing graphs)" ]
            , li [] [ text "Algebraic Graphs (for combining two G-cells together)" ]
            ]
        , h1 [] [ text "2 notes from Zhendong" ]
        , ol []
            [ li []
                [ text "The generality of it: "
                , strong [] [ text "(example with numbers)" ]
                ]
            , li []
                [ text "(Second part of it) Take the vision to its full: "
                , strong [] [ text "Declare computation via demonstration (point-and-click)" ]
                ]
            ]

        --, code [] [ text (output Just (always Nothing) dressUp) ]
        ]


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


viewRecommendationsText rs =
    case rs of
        [] ->
            Html.span [] [ text "no recommendations" ]

        _ ->
            Html.span []
                ([ text "See also: " ]
                    ++ (List.map (\x -> x.other_tag) rs
                            |> List.map (\x -> Html.a [ href "#", onClick (URLSearch x) ] [ text x ])
                            |> List.map (\x -> strong [] [ x, text ", " ])
                            |> List.take 10
                       )
                )
