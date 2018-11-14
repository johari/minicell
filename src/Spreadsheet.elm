port module Spreadsheet exposing (..)

import Debug
import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onDoubleClick, onInput, onBlur, onMouseOver, keyCode, on)
import Html.Attributes exposing (id, class, href, value, autofocus)
import List
import Dict
import Result

import List.Extra exposing (find, updateIf)

import Time

import Spreadsheet.Interpreter.Parser exposing (..)
import Spreadsheet.Types exposing (..)

import Examples.TopoSort exposing (dressUp)
import Spreadsheet.Example exposing (exampleSpreadsheet, exampleSpreadsheetWithGraph, exampleSpreadsheetAdjacencyListWithGraph)

import Graph exposing (Graph, nodes, mapNodes)
import Graph.DOT

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
    | EditIntent CellAddress (Maybe String) -- The string captures the initial value to put to buffer

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

init : () -> ( Model, Cmd Msg )
init _ =
    ( exampleSpreadsheetAdjacencyListWithGraph
    , Cmd.none
    )


-- I don't know why, but this segment of the implementation
-- seems to be a little tricky to implement..
-- You can tell by the weirdness in choosing namings for functions
-- that serve simple purposes.
-- 
-- Maybe it's the most mundane part of the implementation,
-- one that is requiring me, as a programmer, to write boilterplate functions
-- for operations that could be conveniently expressed in a flexible DSL.

-- Weirdness ends near [/weird]

elmIsWeirdWithMaybe newMeta arg = case arg of
    Just e -> Just { e | meta = Just newMeta }
    Nothing -> Nothing


elmIsWeirdWithMaybe2 newValue e = { e | value = newValue }

--updateCellMeta : Database -> CellAddress -> CellMeta -> Database
--updateCellMeta model addr newMeta = Dict.update addr (elmIsWeirdWithMaybe newMeta) model

updateCellValue : Database -> CellAddress -> EExpr -> Database
updateCellValue model addr newValue = updateIf (\x -> x.addr == addr) (elmIsWeirdWithMaybe2 newValue) model


-- ▂▃▅▇█▓▒░ [🔥 updateCellBuffer 💀 folklore 🔥] ░▒▓█▇▅▃▂
--
-- 💀🔥💀🔥💀🔥💀🔥💀🔥💀🔥💀🔥💀🔥💀🔥💀🔥💀🔥💀🔥💀🔥
--
-- I have goofed at implementing `updateCellBuffer` once.
-- It used to be the case that the 
-- type alias Database = Dict.Dict CellAddress Cell
--
-- But then I decided to have
-- type alias Database = List Cell
-- and instead, add an { .. `addr` : CellAddress .. } to the Cell datatype.
-- 
-- The re-factoring sounded straightforward
-- (change `Dict.update`s that took an address,
-- into `updateIf`s that predicate over cell's addr field)
-- but this made it such that no "truly empty cells" could ever fertile.
-- 
-- :(
--
-- The "logic" behind this (once we had a Dict-y Database) was implemented in `elmIsWeirdWithMaybe3`
-- function, which looked like this:
--
--
-- elmIsWeirdWithMaybe3 newBuffer arg = case arg of
--    Just e -> Just { e | buffer = newBuffer }
--    Nothing -> Just { emptyCell | buffer = newBuffer }
--    ^^^^^^^    ^^^^
-- and was called like this:
--
-- updateCellBuffer model addr newValue = Dict.update addr (elmIsWeirdWithMaybe3 newValue) model
--                                        ^^^^^^^^^^^^^^^^
-- I replaced it to this:
--
-- elmIsWeirdWithMaybe3 newBuffer e = { e | buffer = newBuffer }
--                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^
--                  (note how the `Nothing` case was accidentaly eliminated)
--
-- and used like this:
--
-- updateCellBuffer model addr newValue = updateIf (\x -> x.addr == addr) (elmIsWeirdWithMaybe3 newValue) model
--                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--
--
-- I wasn't happy when I refactored the code into a wrong implementation..
-- But I think it was a good call to include the address of the cell
-- into the `Cell` datatype, without requiring to keep two copies
-- of the address of a cell
-- (one for the key of the Dictionary, and the other inside the Cell.. It would've been mayhem 🔥)
--
-- Apparently the first argument of updateCellBuffer must be called "database", not a "model", but
-- luckily the type system made sure the right things are being passed around, despite wrong naming.
--
-- [/folklore]
--
-- 💀🔥💀🔥💀🔥💀🔥💀🔥💀🔥💀🔥💀🔥💀🔥💀🔥💀🔥💀🔥💀🔥

elmIsWeirdWithMaybe3 newBuffer e = { e | buffer = newBuffer }

updateCellBuffer : Database -> CellAddress -> String -> Database
updateCellBuffer database addr newBuffer =
    case find (\x -> x.addr == addr) database of 
        Just cell -> updateIf (\x -> x.addr == addr) (elmIsWeirdWithMaybe3 newBuffer cell |> always) database
        Nothing -> database ++ [{ emptyCell | buffer = newBuffer, addr = addr }]
        

goto direction addr = let (rho, kappa) = addr in (rho+1, kappa)
nudgeRight (rho, kappa) = (rho, kappa+1)
nudgeLeft  (rho, kappa) = (rho, kappa-1)
nudgeDown    (rho, kappa) = (rho-1, kappa)
nudgeUp  (rho, kappa) = (rho+1, kappa)

currentBuffer : Database -> CellAddress -> String
currentBuffer db addr = Maybe.withDefault emptyCell (find (\x -> x.addr == addr) db) |> .buffer

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

-- [/weird]




-- This [update] function is one of our true heroes.. :)

update msg model =
    case msg of
        Tick t ->
            ( { model | currentTime = t }, Cmd.none)

        CollectVertexDemo addr ->
            ( { model | demoVertices = model.demoVertices ++ [(EBot, [])] }, Cmd.none) -- FIXME

        SwitchToMode mode ->
            ({ model | mode = mode }, Cmd.none)

        -- These two actions (EditIntent and UpdateCellBuffer) are tightly [coupled]

        EditIntent addr maybeFirstInput ->

            -- To the future self:
            -- avoid creating the cell in this acition.
            -- Refer to UpdateCellBuffer.

            let newModel =
                    case maybeFirstInput of
                        Just firstInput ->
                            let (v1, v2) = (update (UpdateCellBuffer addr firstInput) model)
                            in
                                v1 -- [update trick], discards the Cmd values :(
                        Nothing -> model
            in
                ( { newModel | mode = EditMode addr }, fixAutoFocusBug cssKeyForEditCellInput)

        UpdateCellBuffer addr newInput ->
            -- When this action is called, we are unsure whether the cell exists in database or not
            --
            -- Please note that UpdateCellBuffer operates independently of the mode we are in..
            -- It feels weird, but you shold not make assumptions about the mode.
            -- Specifically, don't assume you are in EditMode when a buffer changes..
            -- I can't find an easy way to guarantee this.
            --
            ({ model| database = updateCellBuffer model.database addr newInput }, Cmd.none)
            --                                                                    ^^^^^^^^
            --                   If you ever wanted to replace the Cmd.none, consult [update trick].

        -- [/coupled]

        Save addr ->
            let (rho, kappa) = addr in
            ({ model | database = updateCellValue model.database addr (currentBuffer model.database addr |> parseBufferToEExpr model)
                     , mode = IdleMode (rho+1, kappa)
                     }
            , Cmd.none)

        ChangeCandidateCell addr ->
            ( { model | mode = IdleMode addr }, Cmd.none)
        
        -- All the [keyboard magic] happen here
        WindowKeyPress payload ->
            let maybeKey = (Result.toMaybe (Json.decodeValue Json.string payload)) in
                case maybeKey of 
                    Nothing -> (model, Cmd.none)
                    Just key ->
                        case key of
                            "Enter" -> -- [Keypress Enter]
                                case model.mode of 
                                    IdleMode addr -> update (EditIntent addr Nothing) model
                                    EditMode addr -> update (Save addr) model
                                    _ -> (model, Cmd.none)
                            "ArrowRight" -> case model.mode of
                                IdleMode _ -> handleArrowInIdleMode model key
                                _ -> (model, Cmd.none)
                            "ArrowLeft" -> case model.mode of
                                IdleMode _ -> handleArrowInIdleMode model key
                                _ -> (model, Cmd.none)

                            "ArrowDown" ->
                                -- [Keypress Down]
                                case model.mode of
                                    IdleMode _ -> handleArrowInIdleMode model key
                                    _ -> (model, Cmd.none)
                            "ArrowUp" -> case model.mode of
                                IdleMode _ -> handleArrowInIdleMode model key
                                _ -> (model, Cmd.none)
                            _ -> case model.mode of 
                                IdleMode addr ->
                                    if String.length key == 1 then
                                        update (EditIntent addr (Just key)) model
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


-- This is where we get super-detailed about rendering of our widgets

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

                ESuperFancyGraph g ->
                    span [] [ text (g |> mapNodes (\(_, listOfCells) -> List.head listOfCells |> Maybe.withDefault emptyCell |> Debug.toString) |> Graph.DOT.output Just (always Nothing)) ]

                ECellGraph g ->
                    span [] [ text (g |> mapNodes (\cellNode -> cellNode.value |> Debug.toString) |> Graph.DOT.output Just (always Nothing)) ]

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



-- It seems like we don't need this one anymore? (see [Keypress Enter])
onEnter : Msg -> Attribute Msg
onEnter msg =
    on "keydown" <|
        Json.map
            (always msg)
            (keyCode |> Json.andThen (is_ 13))

-- It seems like we don't need this one anymore? (see [Keypress Down])
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
        

--cellBelongsToAGraph addr expr = case expr of case List.find (\v -> v.addr == addr) nodes.graph of 
--    Just v -> True
--    Nothing -> False

-- [note]:
-- If the cell under view is a Graph
-- we want to highlight cells in the spreadsheet that belong to the graph.
-- (This is only useful when we can successfuly extract graphs from the spreadsheet)

computeCellSelectionClass model addr =
    case model.mode of
        IdleMode addrUnderView ->
            if addrUnderView == addr then
                "elm-selected-cell"
            else
                -- Please see [note], then uncomment the following incomplete implementation.
                --
                --let maybeCellUnderViewIsGraph =
                --    find (\v -> v.addr == addrUnderView
                --             && (evaluatesToAGraphWithCellVertices model addrUnderView)
                --             && cellBelongsToAGraph addr
                --in 
                --    case maybeCellUnderViewIsGraph of
                --        Just _ -> "elm-cell-belongs-to-a-graph"
                --        Nothing -> ""
                ""
        _ -> ""

oneCell : CellAddress -> Model -> Html Msg
oneCell addr model =
    if model.mode == EditMode addr then
        td [ ] [ viewCellInEditMode addr (find (\x -> x.addr == addr) model.database) ]
    else
        let possiblyVertexDemo = if model.mode == VertexDemoMode then [onClick (CollectVertexDemo addr)] else [] in
            td ([ onDoubleClick (EditIntent addr Nothing)
                , class (computeCellSelectionClass model addr)
                ]
                ++ possiblyVertexDemo) [ viewCell model (find (\x -> x.addr == addr) model.database) ]

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
