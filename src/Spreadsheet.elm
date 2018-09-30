module Spreadsheet exposing (CellValue(..), Formula, Msg, PointerState(..), Spreadsheet, exampleSpreadsheet, view)

import Browser
import Examples.TopoSort exposing (dressUp)
import Graph exposing (Graph)
import Html exposing (..)
import Html.Attributes exposing (class, href)
import List


type alias Formula =
    String


type alias CellAddress =
    ( Int, Int )


type Msg
    = SelectCell CellAddress -- e.g. Select the first column
    | ModifyCell CellAddress CellValue -- e.g. Change A2 from "foo" to "bar"
    | SelectRange ( CellAddress, CellAddress ) -- e.g. (A2, D5)


type CellValue
    = CellInt Int -- e.g. 42
    | CellGraph (Graph String ()) -- e.g. G = <V, E>
    | CellString String -- e.g. "Hello World!"
    | CellFormula Formula -- e.g. =Dijkstra(G1, "Davis", "Berkeley")
    | CellEmpty -- e.g. ()
    | CellHref String -- e.g. http://cs.tufts.edu/~nr/...
    | HCellList CellValue (List CellValue) -- e.g. Shouldn't this be "HCellList (List CellValue)" instead?



-- Metadata could contain these things:
-- Information for:
--     Highlighting of a cell (also label of the highlights)
--     Class of a cell
--     "grep" of cells
--     ...?


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
            span [] [ text (String.fromInt num) ]

        CellString str ->
            span [] [ text str ]

        CellHref str ->
            a [ href str ] [ text str ]

        CellEmpty ->
            span [] [ text "<empty>" ]

        _ ->
            span [] [ text "rendering not implemented" ]


viewRows table =
    let
        zip =
            List.map2 Tuple.pair
    in
    List.map
        (\( rowNum, tds ) ->
            tr [] ([ td [] [ text (String.fromInt rowNum) ] ] ++ tds)
        )
        (zip (List.range 1 (List.length table)) (List.map listOfTdForRow table))


listOfTdForRow xs =
    List.map (\x -> td [] [ viewCell x ]) xs


topRow =
    tr [ class "top-row" ]
        [ td [] []
        , td [] [ text "A" ]
        , td [] [ text "B" ]
        , td [] [ text "C" ]
        , td [] [ text "D" ]
        , td [] [ text "E" ]
        , td [] [ text "F" ]
        , td [] [ text "G" ]
        , td [] [ text "H" ]
        ]


view : Model -> Html Msg
view model =
    table [ class "spreadsheet" ] ([ topRow ] ++ viewRows model.table)


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
