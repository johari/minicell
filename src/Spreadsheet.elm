port module Spreadsheet exposing (..)

import Debug
import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onDoubleClick, onInput, onBlur, onMouseOver, keyCode, on)
import Html.Attributes exposing (id, class, href, value, autofocus)
import List
import Dict
import Result

import Time

import Spreadsheet.Interpreter.Parser exposing (..)
import Spreadsheet.Types exposing (..)

import Examples.TopoSort exposing (dressUp)

import Graph exposing (Graph, nodes)

import Json.Decode as Json
import Json.Encode as E

port keyPress : (E.Value -> msg) -> Sub msg
port fixAutoFocusBug : String -> Cmd msg

cssKeyForEditCellInput = "magic-input-cell-editor"

type DemonstrationBrush
    = VertexBrush
    | VertexAttributeBrush
    | EdgeBrush
    | EdgeAttributeBrush

type Msg
    = ModifyCell CellAddress EExpr -- e.g. Change A2 from "foo" to "bar"
    | EditIntent CellAddress

    | UpdateCellBuffer CellAddress String
    | Save CellAddress
    | WindowKeyPress E.Value

    | MoveViewCell String
    | ChangeCandidateCell CellAddress

    | SwitchToMode Mode
    | CollectVertexDemo CellAddress
    | Tick Time.Posix
    -- | SelectCell CellAddress -- e.g. Select the first column
    -- | SelectRange ( CellAddress, CellAddress ) -- e.g. (A2, D5)

type alias Model =
    Spreadsheet


el : List ((Int, Int), Cell)

el = [ ( (0, 0), intCell 1 )
     , ( (0, 1), intCell 42 )
     , ( (0, 2), formulaCell "+" [(ECellRef (0, 1)), (ECellRef (0, 0))] )
     , ( (1, 0), graphCell dressUp )
     , ( (3, 3), graphCell dressUp )
     , ( (4, 3), graphCell dressUp )
     , ( (5, 5), stringCell "Hello" )
     , ( (6, 5), stringCell "world!" )
     , ( (6, 6), stringCell "Hello" )
     , ( (7, 6), stringCell "@mysql://nima.wiki/phd/writing_cardbased" )
     ]

exampleSpreadsheet =
    { emptySpreadsheet | database = Dict.fromList el }


init : () -> ( Model, Cmd Msg )
init _ =
    ( exampleSpreadsheet
    , Cmd.none
    )

elmIsWeirdWithMaybe newMeta arg = case arg of
    Just e -> Just { e | meta = Just newMeta }
    Nothing -> Nothing


elmIsWeirdWithMaybe2 newValue arg = case arg of
    Just e -> Just { e | value = newValue }
    Nothing -> Nothing

elmIsWeirdWithMaybe3 newBuffer arg = case arg of
    Just e -> Just { e | buffer = newBuffer }
    Nothing -> Just { emptyCell | buffer = newBuffer }


--updateCellMeta : Database -> CellAddress -> CellMeta -> Database
--updateCellMeta model addr newMeta = Dict.update addr (elmIsWeirdWithMaybe newMeta) model

updateCellValue : Database -> CellAddress -> EExpr -> Database
updateCellValue model addr newValue = Dict.update addr (elmIsWeirdWithMaybe2 newValue) model

updateCellBuffer : Database -> CellAddress -> String -> Database
updateCellBuffer model addr newValue = Dict.update addr (elmIsWeirdWithMaybe3 newValue) model


goto direction addr = let (rho, kappa) = addr in (rho+1, kappa)
nudgeRight (rho, kappa) = (rho, kappa+1)
nudgeLeft  (rho, kappa) = (rho, kappa-1)
nudgeDown    (rho, kappa) = (rho-1, kappa)
nudgeUp  (rho, kappa) = (rho+1, kappa)

currentBuffer : Database -> CellAddress -> String
currentBuffer db addr = Maybe.withDefault emptyCell (Dict.get addr db) |> .buffer

handleArrowInIdleMode model key =
    let
        addr = case model.mode of
                    IdleMode (pos) -> pos
                    _ -> (0, 0)
        nudgeFunction = case key of
                            "ArrowLeft" -> nudgeLeft
                            "ArrowRight" -> nudgeRight
                            "ArrowUp" -> nudgeDown
                            "ArrowDown" -> nudgeUp
                            _ -> (\x -> Debug.log (Debug.toString key) x)
    in
        update (addr |> nudgeFunction |> ChangeCandidateCell) model

                                        

parseBufferToEExpr model buffer = buffer |> stringToEExpr

update msg model =
    case msg of
        Tick t ->
            ( { model | currentTime = t }, Cmd.none)

        CollectVertexDemo addr ->
            ( { model | demoVertices = model.demoVertices ++ [addr] }, Cmd.none)

        SwitchToMode mode ->
            ({ model | mode = mode }, Cmd.none)

        EditIntent addr ->
            ( { model | mode = EditMode addr}
            , fixAutoFocusBug cssKeyForEditCellInput)

        UpdateCellBuffer addr newInput ->
            ({ model| database = updateCellBuffer model.database addr newInput }, Cmd.none)


        Save addr ->
            let (rho, kappa) = addr in
            ({ model | database = updateCellValue model.database addr (currentBuffer model.database addr |> parseBufferToEExpr model)
                     , mode = IdleMode (rho+1, kappa)
                     }
            , Cmd.none)

        ChangeCandidateCell addr ->
            ( { model | mode = IdleMode addr }, Cmd.none)
        
        --MoveViewCell direction -> ...

        WindowKeyPress payload ->
            let maybeKey = (Result.toMaybe (Json.decodeValue Json.string payload)) in
                case maybeKey of 
                    Nothing -> (model, Cmd.none)
                    Just key ->
                        case key of
                            "Enter" -> case model.mode of
                                IdleMode addr -> update (EditIntent addr) model
                                EditMode addr -> update (Save addr) model
                                _ -> (model, Cmd.none)
                            "ArrowRight" -> case model.mode of
                                IdleMode _ -> handleArrowInIdleMode model key
                                _ -> (model, Cmd.none)
                            "ArrowLeft" -> case model.mode of
                                IdleMode _ -> handleArrowInIdleMode model key
                                _ -> (model, Cmd.none)

                            "ArrowDown" -> case model.mode of
                                IdleMode _ -> handleArrowInIdleMode model key
                                _ -> (model, Cmd.none)
                            "ArrowUp" -> case model.mode of
                                IdleMode _ -> handleArrowInIdleMode model key
                                _ -> (model, Cmd.none)
                            _ -> case model.mode of 
                                IdleMode addr ->
                                    if String.length key == 1 then
                                        update (EditIntent addr) { model | database = Dict.update addr (\cell ->
                                            case cell of
                                                Nothing -> Just { emptyCell | buffer = key }
                                                Just c -> Just { c | buffer = key }) model.database }
                                     else
                                        (model, Cmd.none)
                                _ -> (model, Cmd.none)


            --let newCell = { emptyCell | value = CellString )
            --              } in
            --(
            --  { model | database = Dict.insert (0,0) newCell model.database }
            --, Cmd.none
            --)

        --SelectCell loc ->
        --    ( { model | selectionRange = Just ( loc, loc ) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


viewCell : Spreadsheet -> Maybe Cell -> Html msg
viewCell model res =
    case res of
        Nothing -> span [] [ text "" ]
        (Just cell) ->
            case cell.value of
                EILit num ->
                    span [] [ text (String.fromInt num) ]

                ESLit str ->
                    span [] [ text str ]

                EHref str ->
                    a [ href str ] [ text str ]

                EBot ->
                    span [] [ text "()" ]

                EGraph g ->
                    span [] [ text "G = <V, E>", text (g |> nodes |> List.map .id |> Debug.toString) ]

                EApp f args ->
                    let resultOfEvaluation = (eval model cell.value) in
                        viewCell model (Just { cell | value = resultOfEvaluation})

                ECellRef addr ->
                    let resultOfEvaluation = (eval model cell.value) in
                        viewCell model (Just { cell | value = resultOfEvaluation})

                v ->
                    span [] [ text "rendering not implemented" ]


--viewRows table =
--    let
--        zip =
--            List.map2 Tuple.pair
--    in
--    List.map
--        (\( rowNum, tds ) ->
--            tr [] ([ td [] [ text (String.fromInt rowNum) ] ] ++ tds)
--        )
--        (zip (List.range 1 10) (List.map listOfTdForRow table))

--listOfTdForRow xs =
--    List.map (\x -> td [] [ viewCell x ]) xs

onEnter : Msg -> Attribute Msg
onEnter msg =
    on "keydown" <|
        Json.map
            (always msg)
            (keyCode |> Json.andThen (is_ 13))

onArrowDown msg =
    on "keydown" <|
            Json.map
                (always msg)
                (keyCode |> Json.andThen (is_ 40))

is_ : Int -> Int -> Json.Decoder ()
is_ target code =
    if code == target then
        Json.succeed ()
    else
        Json.fail "not the right key code"

viewCellInEditMode addr res  =
    case res of
        (Just cell ) -> input [ value (cell.buffer)
                              , onInput (UpdateCellBuffer addr)
                              , onBlur (Save addr)
                              , autofocus True
                              , id (cssKeyForEditCellInput)
                              ] []
        _ -> viewCellInEditMode addr (Just emptyCell)
        

oneCell : CellAddress -> Model -> Html Msg
oneCell addr model =
    if model.mode == EditMode addr then
        td [ ] [ viewCellInEditMode addr (Dict.get addr model.database) ]
    else
        let possiblyVertexDemo = if model.mode == VertexDemoMode then [onClick (CollectVertexDemo addr)] else [] in
            td ([ onDoubleClick (EditIntent addr)
                , class (if model.mode == IdleMode addr then "elm-selected-cell" else "")
                ]
                ++ possiblyVertexDemo) [ viewCell model (Dict.get addr model.database) ]

viewRow : Int -> Model -> List (Html Msg)
viewRow rho model = [ tr []
                        [ td [] [ text  (rho+1 |> String.fromInt) ]
                        , oneCell (rho, 0) model
                        , oneCell (rho, 1) model
                        , oneCell (rho, 2) model
                        , oneCell (rho, 3) model
                        , oneCell (rho, 4) model
                        , oneCell (rho, 5) model
                        , oneCell (rho, 6) model
                        ]
                    ]

viewRows : Model -> List (Html Msg)
viewRows model = List.range 0 10 |> List.map (\i -> (viewRow i model)) |> List.concat

topRow =
    tr [ class "top-row" ]
        [ td [] [ text " " ]
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
    span [ class (Debug.toString model.mode) ] ([ span [] []
             , button [ id "magic-button-demo-vertex", onClick (SwitchToMode VertexDemoMode) ] [ text "demonstrate vertices" ]
             , button [ id "magic-button-demo-edge", onClick (SwitchToMode EdgeDemoMode) ] [ text "demonstrate edges" ]
             , button [ id "magic-button-generalize", onClick (IdleMode (0, 0) |> SwitchToMode) ] [ text "generalize" ]
             , table [ class "spreadsheet" ] ([ topRow ] ++ viewRows model)
             , hr [] []
             ] ++ (debugView model))

debugView model =
    [ text "mode: ", (text (Debug.toString model.mode))
    , hr [] []
    , text (Debug.toString model) ]

main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [ keyPress WindowKeyPress
                                , Time.every 1000 Tick
                                ]
