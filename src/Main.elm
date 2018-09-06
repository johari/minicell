port module Main exposing (Msg(..), main, update, view)

import Browser
import Graph.DOT as DOT exposing (..)
import Html exposing (Html, b, br, button, code, div, h1, hr, input, li, ol, strong, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as E
import List
import Pinboard as P exposing (..)
import Platform.Cmd exposing (map)
import Stylize exposing (..)


port repaintGraph : String -> Cmd msg


type Msg
    = Increment
    | Decrement
    | NewJSON (Result Http.Error (List Bookmark))
    | URLSearch String


type alias Model =
    { counter : Int
    , urls : List Bookmark
    , description_query : String
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
    ( Model 0 [] "graph"
    , map (\x -> NewJSON x) getBookmarks
    )


update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        URLSearch s ->
            let
                newModel =
                    { model | description_query = s }
            in
            ( newModel, repaintGraph (output Just (always Nothing) (dressUp newModel)) )

        NewJSON result ->
            case result of
                Ok ls ->
                    let
                        newModel =
                            { model | urls = ls }
                    in
                    ( newModel, repaintGraph (output Just (always Nothing) (dressUp newModel)) )

                Err _ ->
                    ( { model | urls = [] }, Cmd.none )


view model =
    div []
        [ button [ onClick Decrement ] [ text "----" ]
        , div [] [ text (String.fromInt model.counter) ]
        , button [ onClick Increment ] [ text "+" ]
        , div [] [ text "this is a test2" ]
        , input [ onInput URLSearch ] []
        , br [] []
        , div [] [ viewBookmarkTable model.urls model.description_query ]
        , br [] []
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
        , code [] [ text (output Just (always Nothing) (dressUp model)) ]
        ]
