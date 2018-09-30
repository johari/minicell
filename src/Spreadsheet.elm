module Spreadsheet exposing (CellValue(..), Formula, Msg, Spreadsheet, exampleSpreadsheet, view)

import Browser
import Examples.TopoSort exposing (dressUp)
import Graph exposing (Graph)
import Html exposing (Html)
import Html.Attributes exposing (class)
import List


type alias Formula =
    String


type alias CellAddress =
    ( Int, Int )


type Msg
    = SelectCell CellAddress
    | ModifyCell CellAddress CellValue
    | SelectRange ( CellAddress, CellAddress )


type CellValue
    = CellInt Int
    | CellGraph (Graph String ())
    | CellString String
    | CellFormula Formula
    | CellEmpty
    | HCellList CellValue (List CellValue)


type alias CellMeta =
    String


type alias Cell =
    ( CellValue, Maybe CellMeta )


type PointerState
    = Idle
    | WithinSelection


type alias Spreadsheet =
    { table : List (List Cell)
    , selectionRange : Maybe ( CellAddress, CellAddress )
    , pointerState : PointerState
    }


type alias Model =
    Spreadsheet


exampleSpreadsheet =
    Spreadsheet
        [ [ ( CellString "Hello", Nothing ), ( CellString "World!", Nothing ) ]
        , [ ( CellGraph dressUp, Nothing ), ( CellInt 42, Nothing ) ]
        ]
        Nothing
        Idle


init : () -> ( Model, Cmd Msg )
init _ =
    ( exampleSpreadsheet
    , Cmd.none
    )


update msg model =
    case msg of
        SelectCell loc ->
            ( { model | selectionRange = Just ( loc, loc ) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


viewCell ( cellValue, _ ) =
    case cellValue of
        CellInt num ->
            Html.span [] [ Html.text (String.fromInt num) ]

        _ ->
            Html.span [] [ Html.text "rendering not implemented" ]


viewRow xs =
    Html.tr [] (List.map (\x -> Html.td [] [ viewCell x ]) xs)


view : Model -> Html Msg
view model =
    Html.table [ class "spreadsheet" ] (List.map viewRow model.table)


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
