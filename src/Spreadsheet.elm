port module Spreadsheet exposing (..)

import Debug
import Browser
import Examples.TopoSort exposing (dressUp)
import Graph exposing (Graph, nodes)
import Html exposing (..)
import Html.Events exposing (onClick, onDoubleClick, onInput, onBlur, onMouseOver, keyCode, on)
import Html.Attributes exposing (id, class, href, value, autofocus)
import List
import Dict
import Result

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

type alias Formula =
    String

type alias CellAddress =
    ( Int, Int )

type Msg
    = ModifyCell CellAddress CellValue -- e.g. Change A2 from "foo" to "bar"
    | EditIntent CellAddress
    | UpdateCellBuffer CellAddress String
    | Save CellAddress
    | WindowKeyPress E.Value

    | MoveViewCell String
    | ChangeCandidateCell CellAddress

    | SwitchToMode Mode
    | CollectVertexDemo CellAddress
    -- | SelectCell CellAddress -- e.g. Select the first column
    -- | SelectRange ( CellAddress, CellAddress ) -- e.g. (A2, D5)


type CellValue
    = CellEmpty -- e.g. ()

    | CellInt Int -- e.g. 42
    
    -- <3 <3 <3
    | CellGraph (Graph String ()) -- e.g. G = <V, E>
    -- <3 <3 <3

    | CellString String -- e.g. "Hello World!"
    | CellFormula Formula -- e.g. =Dijkstra(G1, "Davis", "Berkeley")
    | CellHref String -- e.g. http://cs.tufts.edu/~nr/...
    | HCellList CellValue (List CellValue) -- e.g. Shouldn't this be "HCellList (List CellValue)" instead?

type alias CellMeta =
    String


type alias Cell =
    { value : CellValue
    , buffer : String 
    -- Each time you edit a cell, you are modifying the "buffer". 
    -- Once you press enter the buffer will be parsed, and we replace the "value" attribute with
    -- the result of the parser
    --
    -- the logic for the evaluator can be traced in the code that renders the cells to HTML
    , meta  : Maybe CellMeta
    }

emptyCell = Cell CellEmpty "" Nothing
stringCell str = { emptyCell | value = CellString str }
intCell i = { emptyCell | value = CellInt i }
graphCell g = { emptyCell | value = CellGraph dressUp }

type Mode
    = IdleMode
    | EditMode
    | VertexDemoMode
    | EdgeDemoMode

-- is this necessary?
toString a = case a of
    IdleMode -> "IdleMode"
    EditMode -> "EditMode"
    VertexDemoMode -> "Vertex Demonstration Mode"
    EdgeDemoMode -> "Edge Demonstration Mode"
    --_        -> "Some other mode"

type alias Database = Dict.Dict (Int, Int) Cell

type alias Spreadsheet =
    { database : Database
    --, selectionRange : Maybe ( CellAddress, CellAddress )
    , mode : Mode
    , cellUnderModification : Maybe CellAddress
    , cellUnderView : Maybe CellAddress
    , demoVertices : List CellAddress
    , demoEdges : List (CellAddress, CellAddress)
    }


type alias Model =
    Spreadsheet


el : List ((Int, Int), Cell)

el = [ ( (0, 0), stringCell "Hello" )
     , ( (0, 1), stringCell "world!" )
     , ( (1, 0), graphCell dressUp )
     , ( (1, 1), intCell 42 )
     , ( (3, 3), graphCell dressUp )
     , ( (4, 3), graphCell dressUp )
     , ( (5, 5), stringCell "Hello" )
     , ( (6, 5), stringCell "world!" )
     , ( (6, 6), stringCell "Hello" )
     , ( (7, 6), stringCell "@mysql://nima.wiki/phd/writing_cardbased" )
     ]

exampleSpreadsheet =
    Spreadsheet (Dict.fromList el) (IdleMode) (Nothing) (Just (0,0)) [] []


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

updateCellValue : Database -> CellAddress -> CellValue -> Database
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
        addr = model.cellUnderView |> Maybe.withDefault (0,0)
        nudgeFunction = case key of
                            "ArrowLeft" -> nudgeLeft
                            "ArrowRight" -> nudgeRight
                            "ArrowUp" -> nudgeDown
                            "ArrowDown" -> nudgeUp
                            _ -> (\x -> Debug.log (Debug.toString key) x)
    in
        update (addr |> nudgeFunction |> ChangeCandidateCell) model

                                        

parseBufferToCellValue x = CellString ("I need to learn how to parse " ++ (Debug.toString x))
update msg model =
    case msg of
        CollectVertexDemo addr ->
            ( { model | demoVertices = model.demoVertices ++ [addr] }, Cmd.none)

        SwitchToMode mode ->
            ({ model | mode = mode }, Cmd.none)

        EditIntent addr ->
            ( { model | cellUnderModification = Just addr
                      , mode = EditMode}
            , fixAutoFocusBug cssKeyForEditCellInput)
        UpdateCellBuffer addr newInput ->
            ({ model| database = updateCellBuffer model.database addr newInput }, Cmd.none)
        Save addr ->
            ({ model | cellUnderModification = Nothing
                     , database = updateCellValue model.database addr (currentBuffer model.database addr |> parseBufferToCellValue)
                     , mode = IdleMode
                     }
            , Cmd.none)

        ChangeCandidateCell addr ->
            ( { model | cellUnderView = Just addr }, Cmd.none)
        
        --MoveViewCell direction -> ...

        WindowKeyPress payload ->
            let maybeKey = (Result.toMaybe (Json.decodeValue Json.string payload)) in
                case maybeKey of 
                    Nothing -> (model, Cmd.none)
                    Just key ->
                        case key of
                            "Enter" -> case model.mode of
                                IdleMode ->
                                    case model.cellUnderView of
                                        (Just addr) -> update (EditIntent addr) model
                                        _ -> (model, Cmd.none)
                                EditMode ->
                                    case model.cellUnderModification of
                                        Just addr -> let (newModel, cmd) = update (Save addr) model in
                                            ({newModel | cellUnderView = addr |> \(x, y) -> Just (x+1, y) }, cmd)
                                        _ -> (model, Cmd.none)
                                _ -> (model, Cmd.none)
                            "ArrowRight" -> case model.mode of
                                IdleMode -> handleArrowInIdleMode model key
                                _ -> (model, Cmd.none)
                            "ArrowLeft" -> case model.mode of
                                IdleMode -> handleArrowInIdleMode model key
                                _ -> (model, Cmd.none)

                            "ArrowDown" -> case model.mode of
                                IdleMode -> handleArrowInIdleMode model key
                                _ -> (model, Cmd.none)
                            "ArrowUp" -> case model.mode of
                                IdleMode -> handleArrowInIdleMode model key
                                _ -> (model, Cmd.none)
                            _ -> case model.mode of 
                                IdleMode ->
                                    if String.length key == 1 then
                                        let addr = model.cellUnderView |> Maybe.withDefault (0,0) in
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


viewCell res =
    case res of
        Nothing -> span [] [ text "" ]
        (Just cell) ->
            case cell.value of
                CellInt num ->
                    span [] [ text (String.fromInt num) ]

                CellString str ->
                    span [] [ text str ]

                CellHref str ->
                    a [ href str ] [ text str ]

                CellEmpty ->
                    span [] [ text "()" ]

                CellGraph g ->
                    span [] [ text "G = <V, E>", text (g |> nodes |> List.map .id |> Debug.toString) ]

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
    if model.cellUnderModification == Just addr then
        td [ ] [ viewCellInEditMode addr (Dict.get addr model.database) ]
    else
        let possiblyVertexDemo = if model.mode == VertexDemoMode then [onClick (CollectVertexDemo addr)] else [] in
            td ([ onDoubleClick (EditIntent addr)
                , class (if model.cellUnderView == Just addr then "elm-selected-cell" else "")
                ]
                ++ possiblyVertexDemo) [ viewCell (Dict.get addr model.database) ]

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
             , button [ id "magic-button-generalize", onClick (SwitchToMode IdleMode) ] [ text "generalize" ]
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
subscriptions model = keyPress WindowKeyPress
