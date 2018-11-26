port module Spreadsheet exposing (..)

import Debug
import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onDoubleClick, onInput, onBlur, onMouseOver, keyCode, on)
import Html.Attributes exposing (id, class, href, value, autofocus, src)
import List
import Dict
import Result

import String exposing (fromInt)

import List.Extra exposing (find, updateIf, elemIndex)

import Time

import Spreadsheet.Interpreter.Parser exposing (..)
import Spreadsheet.Types exposing (..)

import Examples.TopoSort exposing (dressUp)
import Spreadsheet.Example exposing (exampleSpreadsheet, exampleSpreadsheetWithGraph, exampleSpreadsheetAdjacencyListWithGraph)

import Spreadsheet.Wrangling.AdjacencyMatrix as AM

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
    = FlushRegister CellAddress EExpr -- e.g. Change A2 from "foo" to "bar"
    | EditIntent CellAddress (Maybe String) -- The string captures the initial value to put to buffer

    | UpdateCellBuffer CellAddress String
    | Save CellAddress
    | WindowKeyPress E.Value

    | MoveViewCell String
    | ChangeCandidateCell CellAddress

    | SwitchToMode Mode
    | CollectVertexDemo CellAddress
    
    | CollectEdgeDemo1 CellAddress
    | CollectEdgeDemo2 VertexAndPerhapsCells CellAddress

    | Tick Time.Posix

    | SwitchSpreadsheet Spreadsheet

    | ExtractGraphFromIncidenceMatrix
    -- | SelectCell CellAddress -- e.g. Select the first column
    -- | SelectRange ( CellAddress, CellAddress ) -- e.g. (A2, D5)

type alias Model =
    Spreadsheet

init : () -> ( Model, Cmd Msg )
init _ =
    ( exampleSpreadsheet
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
updateCellValue database addr newValue = 
    case find (\x -> x.addr == addr) database of
        Just cell -> updateIf (\x -> x.addr == addr) (elmIsWeirdWithMaybe2 newValue) database
        Nothing -> database ++ [{ emptyCell | value = newValue, addr = addr }]


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
        FlushRegister addr expr ->
            ({ model | database = updateCellValue model.database addr expr
                     , mode = IdleMode addr
                     }
            , Cmd.none)

        ExtractGraphFromIncidenceMatrix ->
            let
                -- Elm compiler crashed if the following line was present
                --formula = (AM.canonicalMatrixWrangler model.database) |> ESuperFancyGraph
                formula = EBot
            in
                ( { model | mode = RegisterFlushMode (((AM.canonicalMatrixWrangler model.database) |> ESuperFancyGraph)) }, Cmd.none )

        SwitchSpreadsheet db ->
            ( db, Cmd.none )
        Tick t ->
            ( { model | currentTime = t }, Cmd.none)

        CollectVertexDemo addr ->
            if addrInVertexDemo addr model.demoVertices then
                -- Remove the address from the demo vertices
                ( { model | demoVertices = removeAddrFromDemoVertices addr model.demoVertices}, Cmd.none )
            else
                case find (\x -> x.addr == addr) model.database of
                    Just cell -> 
                        ( { model | demoVertices = model.demoVertices ++ [(cell.value, [cell])] }, Cmd.none)
                    Nothing ->
                        -- An empty cell cannot be a demonstration!
                        ( model, Cmd.none )

        CollectEdgeDemo1 addr ->
            case getCellVertex addr model.demoVertices of
                [] -> (model, Cmd.none)
                -- Switch to mode that collects the (2/2) vertex
                --(keeping the one that was just shown in its pocket)
                (cellVertex::_) ->
                    ( { model | mode = EdgeDemoMode2 cellVertex }, Cmd.none)
                

        CollectEdgeDemo2 cellVertex1 addr2 ->
            case getCellVertex addr2 model.demoVertices of
                (cellVertex2::_) ->
                    let newSuperEdge = (cellVertex1, cellVertex2, Nothing)
                        newModel = ( { model | demoEdges = model.demoEdges ++ [ newSuperEdge ]
                                             , mode = EdgeDemoMode1
                                     } )
                    in
                        -- add edge demonstration (addr1, addr2) into the list of demonstrations
                        
                        -- goto the step that collects (1/2 vertex)
                        (newModel, Cmd.none)
                [] ->
                    (model, Cmd.none)

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
                    let
                        l1 = Graph.nodes g |> List.length
                        l2 = Graph.edges g |> List.length
                        textValue = "Graph of size " ++ (fromInt l1) ++ ", with " ++ (fromInt l2) ++ " edges"
                        textValue2 = 
                            (g
                                |> mapNodes (\(_, listOfCells) -> 
                                    List.head listOfCells
                                        |> Maybe.withDefault emptyCell
                                        |> Debug.toString)
                                |> Graph.DOT.output Just (always Nothing))
                    in
                        span [] [ text textValue ]

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
        VertexDemoMode ->
            -- If the passed address is in our demo, we want to distinguish it.
            if addrInVertexDemo addr model.demoVertices then "elm-cell-part-of-primary-demonstration" else ""
        EdgeDemoMode1 -> 
            if addrInVertexDemo addr model.demoVertices then "elm-cell-part-of-secondary-demonstration" else ""
        EdgeDemoMode2 cellVertex -> 
            if addrInVertexDemo addr model.demoVertices || addrInVertexDemo addr [ cellVertex ] then "elm-cell-part-of-secondary-demonstration" else ""
        _ -> ""

oneCell : CellAddress -> Model -> Html Msg
oneCell addr model =
    if model.mode == EditMode addr then
        td [ ] [ viewCellInEditMode addr (find (\x -> x.addr == addr) model.database) ]
    else if model.mode == VertexDemoMode then
        td [ onClick (CollectVertexDemo addr)
           , class (computeCellSelectionClass model addr)
           ]
           [ viewCell model (find (\x -> x.addr == addr) model.database) ]
    else if model.mode == EdgeDemoMode1 then
        td [ onClick (CollectEdgeDemo1 addr)
           , class (computeCellSelectionClass model addr)
           ]
           [ viewCell model (find (\x -> x.addr == addr) model.database) ]
    else
        case model.mode of
            EdgeDemoMode2 cellVertex ->
                td [ onClick (CollectEdgeDemo2 cellVertex addr)
                   , class (computeCellSelectionClass model addr)
                   ]
                   [ viewCell model (find (\x -> x.addr == addr) model.database) ]
            _ ->
                let clickActions =
                        case model.mode of
                            RegisterFlushMode expr ->
                                [ onClick (FlushRegister addr expr) ]
                            _ -> [ onDoubleClick (EditIntent addr Nothing) ]
                in
                    td ([ class (computeCellSelectionClass model addr) ] ++ clickActions)
                       [ viewCell model (find (\x -> x.addr == addr) model.database) ]

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
                        , oneCell (rho, 7) model
                        ]
                    ]

viewRows : Model -> List (Html Msg)
viewRows model = List.range 0 10 |> List.map (\i -> (viewRow i model)) |> List.concat

calculateClassForRow rowLabel model = 
    case model.mode of
        IdleMode (rho, kappa) ->
            if elemIndex rowLabel ["A", "B", "C", "D", "E", "F", "G", "H", "I"] == Just kappa then
                class "elm-selected-column"
            else
                class ""
        _ -> class ""

topRow model =
    tr [ class "top-row" ]
        [ td [ ] [ text " " ]
        , td [ calculateClassForRow "A" model ] [ text "A" ]
        , td [ calculateClassForRow "B" model ] [ text "B" ]
        , td [ calculateClassForRow "C" model ] [ text "C" ]
        , td [ calculateClassForRow "D" model ] [ text "D" ]
        , td [ calculateClassForRow "E" model ] [ text "E" ]
        , td [ calculateClassForRow "F" model ] [ text "F" ]
        , td [ calculateClassForRow "G" model ] [ text "G" ]
        , td [ calculateClassForRow "H" model ] [ text "H" ]
        ]

clippy : Model -> Html Msg
clippy model = 
    case model.mode of
        IdleMode _ ->
            span [ ] [ text "👁 Try navigating to a new cell by using arrow keys. Press enter or start typing to edit a cell"]
        EditMode _ ->
            span [] [ text "Try typing in a formula like (=1+1)"]
        VertexDemoMode ->
            span [] [ text "Show me an example of a few nodes, then click on \"Demonstrate Edges\" once you are done. :)"]
        EdgeDemoMode1 ->
            let message = case List.length model.demoEdges of
                            0 -> "Start providing a demonstration of an edge by clicking on a source vertex"
                            1 -> "Provide more edges, or click _Done_"
                            _ -> "If you are happy with the provided demonstration, press _generalize_"
            in
                span [] [ text message ]
        EdgeDemoMode2 _ ->
            span [] [ text "... click on the second vertex to finalize edge demonstration"]
        RegisterFlushMode _ ->
            span [] [ text "click on the desired cell to replace its content" ]
        -- _ -> span [ ] [ text "I'm not trained to assist you in this mode :(" ]

loadExampleButtons =
    div [ id "container-examples" ] [
      button [ onClick (SwitchSpreadsheet exampleSpreadsheet) ]
        [ text "Example 1: Matrix" ]
    , button [ onClick (SwitchSpreadsheet exampleSpreadsheetWithGraph) ]
        [ text "Example 2: Matrix with Graph" ]
    , button [ onClick (SwitchSpreadsheet exampleSpreadsheetAdjacencyListWithGraph) ]
        [ text "Example 3: Adjacency list with Graph" ]
    ]

vertexDemoButtons model = div [ id "container-demo-buttons" ] [
      button [ id "magic-button-demo-vertex", onClick (SwitchToMode VertexDemoMode) ]
       [ text ("demonstrate vertices (" ++ (model.demoVertices |> List.length |> Debug.toString) ++ ")") ]
    --, button [ id "magic-button-demo-edge", onClick (SwitchToMode EdgeDemoMode1) ]
    --   --[ text "demonstrate edges" ]
    --   [ text ("demonstrate edges (" ++ (model.demoEdges |> List.length |> Debug.toString) ++ ")") ]
    , button [ id "magic-button-generalize", onClick (IdleMode (0, 0) |> SwitchToMode) ] [ text "generalize" ]
    ]

graphExtractionButtons model = div [ id "container-demo-buttons" ]
    [ button [ onClick ExtractGraphFromIncidenceMatrix ] [ text "extract a weighted graph from incidence matrix" ]
    , button [ ] [ text "extract graph from adjacency list" ]
    ]

spreadsheetInterface model = 
    div [ id "container-spreadsheet" ] [
        table [ class "spreadsheet" ] ([ topRow model ] ++ viewRows model)
    ]

mainInterface model =
    span [ id "container-content" ] [
      span [] []
    , div [ ] [ loadExampleButtons ]
    , div [ id "container-minicell-logo" ]
        [ img [ src "https://nima.wiki//resources/assets/wiki.png" ] []
        , text "Minicell" 
        , sub [ class "minicell-version" ] [ text "(Version 0.1.0)" ]
        ]
    --, div [ ] [ vertexDemoButtons model ]
    , div [ ] [ graphExtractionButtons model ]
    , spreadsheetInterface model
    ]

footerContent model =
    table [] [
        tr [] [
          td [ id "clippy" ] [ clippy model ] -- Summon Clippy
        , td [ ] [ text (Debug.toString model.mode) ]
        ]
    ]

view : Model -> Html Msg
view model =
    div [ id "container-app" ]
            ([ mainInterface model
             ] ++
             [ footer [] [ footerContent model ] ]
             --++ (debugView model)
             )

debugView model =
    [
        code []
            [ text "mode: ", (text (Debug.toString model.mode))
            , hr [] []
            , text (Debug.toString model) ]
    ]
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
