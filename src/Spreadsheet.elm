port module Spreadsheet exposing (..)

import Browser
import Browser.Navigation
import Debug
import Dict
import Examples.TopoSort exposing (dressUp)
import File exposing (File)
import Graph exposing (Graph, mapNodes, nodes)
import Graph.DOT
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, controls, height, href, id, property, src, style, value, width)
import Html.Events exposing (keyCode, on, onBlur, onClick, onDoubleClick, onInput, onMouseOver, preventDefaultOn)
import Html.Parser
import Html.Parser.Util
import Http
import Json.Decode as D
import Json.Encode as E
import List
import List.Extra exposing (elemIndex, find, updateIf)
import Result
import Spreadsheet.Example exposing (..)
import Spreadsheet.Interpreter.Parser exposing (..)
import Spreadsheet.Types exposing (..)
import Spreadsheet.Wrangling.AdjacencyMatrix as AM
import String exposing (fromInt)
import Task
import Time
import Url


port keyPress : (E.Value -> msg) -> Sub msg


port fixAutoFocusBug : String -> Cmd msg


cssKeyForEditCellInput =
    "magic-input-cell-editor"


preferences =
    { vimMode = True
    }


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
    | DragEnter CellAddress
    | DragLeave
    | GotFiles CellAddress File (List File)
    | GotPreviews CellAddress (List String)
    | CometUpdate CometKey (Result Http.Error E.Value)
    | CometUpdateAll (Result Http.Error E.Value)
    | NoOp



-- | SelectCell CellAddress -- e.g. Select the first column
-- | SelectRange ( CellAddress, CellAddress ) -- e.g. (A2, D5)
-- FIXME: The implementation of comeKeyToAddr only support A{1-99}!!


cometKeyToAddr cometKey =
    case String.toList cometKey of
        kappaC :: [ rhoC ] ->
            ( ((rhoC |> Char.toUpper |> Char.toCode) - Char.toCode '0') - 1
            , (kappaC |> Char.toUpper |> Char.toCode) - Char.toCode 'A'
            )

        kappaC :: rhoC1 :: [ rhoC2 ] ->
            ( ((rhoC1 |> Char.toUpper |> Char.toCode) - Char.toCode '0') * 10 + ((rhoC2 |> Char.toUpper |> Char.toCode) - Char.toCode '0') - 1
            , (kappaC |> Char.toUpper |> Char.toCode) - Char.toCode 'A'
            )

        _ ->
            ( 0, 0 )


minicellEndpoint : Model -> Endpoint -> String
minicellEndpoint model ep =
    case Url.fromString model.location of
        Nothing ->
            ""

        Just url ->
            case String.toList url.path of
                '/' :: '*' :: rest ->
                    String.fromList ('/' :: '_' :: rest)

                _ ->
                    case ep of
                        EPAll ->
                            "/minicell/all.json"

                        EPWrite ck ->
                            "/minicell/" ++ ck ++ "/write.json"

                        EPShow ck ->
                            "/minicell/" ++ ck ++ "/show.json"


addrToExcelStyle addr =
    let
        ( rho, kappa ) =
            addr

        columnString =
            Char.fromCode (kappa + Char.toCode 'A') |> String.fromChar
    in
    columnString ++ String.fromInt (rho + 1)


curlXPOST model addr formula =
    Http.post
        { url = minicellEndpoint model (EPWrite (addrToExcelStyle addr))
        , body = postBodyUpdateFormula formula
        , expect = Http.expectJson (CometUpdate (addrToExcelStyle addr)) D.value
        }


postBodyUpdateFormula formula =
    Http.multipartBody
        [ Http.stringPart "user" formula
        ]


type alias Model =
    Spreadsheet


initWithLocation : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
initWithLocation _ url _ =
    let
        model =
            { emptySpreadsheet | location = Url.toString url }
    in
    ( model, Cmd.batch [ cometUpdateAll model ] )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            emptySpreadsheet
    in
    ( model
    , Cmd.batch
        [ cometUpdateAll model
        ]
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


elmIsWeirdWithMaybe newMeta arg =
    case arg of
        Just e ->
            Just { e | meta = Just newMeta }

        Nothing ->
            Nothing


elmIsWeirdWithMaybe2 newValue e =
    { e | value = newValue }



--updateCellMeta : Database -> CellAddress -> CellMeta -> Database
--updateCellMeta model addr newMeta = Dict.update addr (elmIsWeirdWithMaybe newMeta) model
-- This is a very useful function now that we replaced Dict with List


updateCellValue : Database -> CellAddress -> EExpr -> Database
updateCellValue database addr newValue =
    case find (\x -> x.addr == addr) database of
        Just cell ->
            updateIf (\x -> x.addr == addr) (elmIsWeirdWithMaybe2 newValue) database

        Nothing ->
            database ++ [ { emptyCell | value = newValue, addr = addr } ]



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


elmIsWeirdWithMaybe3 newBuffer e =
    { e | buffer = newBuffer }


updateCellBuffer : Database -> CellAddress -> String -> Database
updateCellBuffer database addr newBuffer =
    case find (\x -> x.addr == addr) database of
        Just cell ->
            updateIf (\x -> x.addr == addr) (elmIsWeirdWithMaybe3 newBuffer cell |> always) database

        Nothing ->
            database ++ [ { emptyCell | buffer = newBuffer, addr = addr } ]


goto direction addr =
    let
        ( rho, kappa ) =
            addr
    in
    ( rho + 1, kappa )


nudgeRight ( rho, kappa ) =
    ( rho, kappa + 1 )


nudgeLeft ( rho, kappa ) =
    ( rho, kappa - 1 )


nudgeDown ( rho, kappa ) =
    ( rho - 1, kappa )


nudgeUp ( rho, kappa ) =
    ( rho + 1, kappa )


currentBuffer : Database -> CellAddress -> String
currentBuffer db addr =
    Maybe.withDefault emptyCell (find (\x -> x.addr == addr) db) |> .buffer


handleArrowInIdleMode model key =
    let
        addr =
            case model.mode of
                IdleMode pos ->
                    pos

                _ ->
                    ( 0, 0 )

        nudgeFunction =
            case key of
                "ArrowLeft" ->
                    nudgeLeft

                "ArrowRight" ->
                    nudgeRight

                "ArrowUp" ->
                    nudgeDown

                "ArrowDown" ->
                    nudgeUp

                _ ->
                    \x -> Debug.log (Debug.toString key) x
    in
    update (addr |> nudgeFunction |> ChangeCandidateCell) model


parseBufferToEExpr model buffer =
    buffer |> stringToEExpr



-- [/weird]


retrieveOldModeOrIdleAtTheOrigin mode =
    case mode of
        FileDropMode prevMode _ ->
            prevMode

        _ ->
            IdleMode ( 0, 0 )



-- This [update] function is one of our true heroes.. :)


update msg model =
    case msg of
        DragEnter addr ->
            case model.mode of
                FileDropMode prevMode _ ->
                    ( { model | mode = FileDropMode prevMode addr }, Cmd.none )

                _ ->
                    ( { model | mode = FileDropMode model.mode addr }, Cmd.none )

        DragLeave ->
            ( { model | mode = retrieveOldModeOrIdleAtTheOrigin model.mode }, Cmd.none )

        GotFiles addr file files ->
            ( { model | mode = retrieveOldModeOrIdleAtTheOrigin model.mode }
            , Task.perform (GotPreviews addr) <|
                Task.sequence <|
                    List.map File.toUrl (file :: files)
            )

        GotPreviews addr urls ->
            ( { model | database = updateCellValue model.database addr (EImage (Maybe.withDefault mondrianSrc (List.head urls))) }
            , Cmd.none
            )

        FlushRegister addr expr ->
            ( { model
                | database = updateCellValue model.database addr expr
                , mode = IdleMode addr
              }
            , Cmd.none
            )

        ExtractGraphFromIncidenceMatrix ->
            let
                -- Elm compiler crashed if the following line was present
                --formula = (AM.canonicalMatrixWrangler model.database) |> ESuperFancyGraph
                formula =
                    EBot
            in
            ( { model | mode = RegisterFlushMode (AM.canonicalMatrixWrangler model.database |> ESuperFancyGraph) }, Cmd.none )

        SwitchSpreadsheet db ->
            ( db, Cmd.none )

        Tick t ->
            case model.mode of
                EditMode _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | currentTime = t }, cometUpdateAll model )

        CollectVertexDemo addr ->
            if addrInVertexDemo addr model.demoVertices then
                -- Remove the address from the demo vertices
                ( { model | demoVertices = removeAddrFromDemoVertices addr model.demoVertices }, Cmd.none )

            else
                case find (\x -> x.addr == addr) model.database of
                    Just cell ->
                        ( { model | demoVertices = model.demoVertices ++ [ ( cell.value, [ cell ] ) ] }, Cmd.none )

                    Nothing ->
                        -- An empty cell cannot be a demonstration!
                        ( model, Cmd.none )

        CollectEdgeDemo1 addr ->
            case getCellVertex addr model.demoVertices of
                [] ->
                    ( model, Cmd.none )

                -- Switch to mode that collects the (2/2) vertex
                --(keeping the one that was just shown in its pocket)
                cellVertex :: _ ->
                    ( { model | mode = EdgeDemoMode2 cellVertex }, Cmd.none )

        CollectEdgeDemo2 cellVertex1 addr2 ->
            case getCellVertex addr2 model.demoVertices of
                cellVertex2 :: _ ->
                    let
                        newSuperEdge =
                            ( cellVertex1, cellVertex2, Nothing )

                        newModel =
                            { model
                                | demoEdges = model.demoEdges ++ [ newSuperEdge ]
                                , mode = EdgeDemoMode1
                            }
                    in
                    -- add edge demonstration (addr1, addr2) into the list of demonstrations
                    -- goto the step that collects (1/2 vertex)
                    ( newModel, Cmd.none )

                [] ->
                    ( model, Cmd.none )

        SwitchToMode mode ->
            ( { model | mode = mode }, Cmd.none )

        -- These two actions (EditIntent and UpdateCellBuffer) are tightly [coupled]
        EditIntent addr maybeFirstInput ->
            -- To the future self:
            -- avoid creating the cell in this acition.
            -- Refer to UpdateCellBuffer.
            let
                newModel =
                    case maybeFirstInput of
                        Just firstInput ->
                            let
                                ( v1, v2 ) =
                                    update (UpdateCellBuffer addr firstInput) model
                            in
                            v1

                        -- [update trick], discards the Cmd values :(
                        Nothing ->
                            model
            in
            ( { newModel | mode = EditMode addr }, fixAutoFocusBug cssKeyForEditCellInput )

        UpdateCellBuffer addr newInput ->
            -- When this action is called, we are unsure whether the cell exists in database or not
            --
            -- Please note that UpdateCellBuffer operates independently of the mode we are in..
            -- It feels weird, but you shold not make assumptions about the mode.
            -- Specifically, don't assume you are in EditMode when a buffer changes..
            -- I can't find an easy way to guarantee this.
            --
            ( { model | database = updateCellBuffer model.database addr newInput }, Cmd.none )

        --                                                                    ^^^^^^^^
        --                   If you ever wanted to replace the Cmd.none, consult [update trick].
        -- [/coupled]
        Save addr ->
            let
                ( rho, kappa ) =
                    addr
            in
            ( { model
                | database = updateCellValue model.database addr (addrToExcelStyle addr |> EComet)
                , mode = IdleMode ( rho + 1, kappa )
              }
            , curlXPOST model addr (currentBuffer model.database addr)
            )

        ChangeCandidateCell addr ->
            ( { model | mode = IdleMode addr }, Cmd.none )

        CometUpdate cometKey res ->
            case res of
                Ok payload ->
                    let
                        ( _, ( fx, val ) ) =
                            decodeCometValue payload
                    in
                    --( { model | cometStorage = Dict.insert cometKey (Debug.log (Debug.toString cometKey) val) model.cometStorage}, Cmd.none )
                    ( { model | cometStorage = Dict.insert cometKey ( "", val ) model.cometStorage }, cometUpdateAll model )

                Err err ->
                    ( { model | cometStorage = Dict.insert cometKey ( "", Debug.toString err |> EError ) model.cometStorage }, Cmd.none )

        CometUpdateAll res ->
            {-
               exampleTableRemote =
               [ cometCell (0, 0) "A1", cometCell (1, 0) "A2", cometCell (2, 0) "A3", cometCell (3, 0) "A4", cometCell (4, 0) "A5", cometCell (5, 0) "A6", cometCell (6, 0) "A7", cometCell (7, 0) "A8", cometCell (8, 0) "A9", cometCell (9, 0) "A10"
               , cometCell (0, 1) "B1", cometCell (1, 1) "B2", cometCell (2, 1) "B3", cometCell (3, 1) "B4"
               , cometCell (0, 2) "C1", cometCell (1, 2) "C2", cometCell (2, 2) "C3", cometCell (3, 2) "C4"
               ]
            -}
            case res of
                Ok payload ->
                    --el = D.decodeValue (D.list (D.field "valueType" D.string)) payload
                    case D.decodeValue (D.list D.value) payload of
                        Ok el ->
                            let
                                keyValPairs =
                                    List.map decodeCometValue el

                                newDict =
                                    Dict.fromList (Dict.toList model.cometStorage ++ keyValPairs)

                                newDatabase =
                                    List.map (\( cometKey, ( fx, val ) ) -> { emptyCell | addr = cometKeyToAddr cometKey, value = val, formula = fx, buffer = fx })
                                        keyValPairs
                            in
                            --( { model | cometStorage = newDict, database = newDatabase }, Debug.log (Debug.toString newDict) Cmd.none)
                            ( { model | cometStorage = newDict, database = newDatabase }, Cmd.none )

                        Err err ->
                            ( Debug.log (Debug.toString err) model, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        -- All the [keyboard magic] happen here
        WindowKeyPress payload ->
            let
                maybeKey =
                    Result.toMaybe (D.decodeValue D.string payload)
            in
            case maybeKey of
                Nothing ->
                    ( model, Cmd.none )

                Just key ->
                    case key of
                        "Enter" ->
                            -- [Keypress Enter]
                            case model.mode of
                                IdleMode addr ->
                                    case find (\x -> x.addr == addr) model.database of
                                        Just cell ->
                                            case cell.value of
                                                EJumpLink _ src ->
                                                    ( model, Browser.Navigation.load src )

                                                _ ->
                                                    update (EditIntent addr Nothing) model

                                        Nothing ->
                                            update (EditIntent addr Nothing) model

                                EditMode addr ->
                                    update (Save addr) model

                                _ ->
                                    ( model, Cmd.none )

                        "ArrowRight" ->
                            case model.mode of
                                IdleMode _ ->
                                    handleArrowInIdleMode model key

                                _ ->
                                    ( model, Cmd.none )

                        "ArrowLeft" ->
                            case model.mode of
                                IdleMode _ ->
                                    handleArrowInIdleMode model key

                                _ ->
                                    ( model, Cmd.none )

                        "ArrowDown" ->
                            -- [Keypress Down]
                            case model.mode of
                                IdleMode _ ->
                                    handleArrowInIdleMode model key

                                _ ->
                                    ( model, Cmd.none )

                        "ArrowUp" ->
                            case model.mode of
                                IdleMode _ ->
                                    handleArrowInIdleMode model key

                                _ ->
                                    ( model, Cmd.none )

                        _ ->
                            case model.mode of
                                IdleMode addr ->
                                    if preferences.vimMode then
                                        -- VIM shortcuts
                                        if key == "=" then
                                            update (EditIntent addr (Just key)) model

                                        else if key == "j" then
                                            handleArrowInIdleMode model "ArrowDown"

                                        else if key == "k" then
                                            handleArrowInIdleMode model "ArrowUp"

                                        else if key == "l" then
                                            handleArrowInIdleMode model "ArrowRight"

                                        else if key == "h" then
                                            handleArrowInIdleMode model "ArrowLeft"

                                        else if String.length key == 1 then
                                            update (EditIntent addr (Just key)) model

                                        else
                                            ( model, Cmd.none )

                                    else if String.length key == 1 then
                                        update (EditIntent addr (Just key)) model

                                    else
                                        ( model, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

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



-- This method tries to parse cometKey


decodeCometValue payload =
    let
        perhapsFx =
            D.decodeValue (D.field "formula" D.string) payload

        fx =
            case perhapsFx of
                Ok str ->
                    str

                Err _ ->
                    "no formula"

        perhapsCometKey =
            D.decodeValue (D.field "cometKey" D.string) payload

        cometKey =
            case perhapsCometKey of
                Ok str ->
                    str

                Err _ ->
                    "undefined"

        -- FIXME
    in
    ( cometKey, ( fx, cometValueTOEExpr payload ) )



-- This ignores cometKey


cometValueTOEExpr payload =
    let
        valueType =
            D.decodeValue (D.field "valueType" D.string) payload
    in
    case valueType of
        Ok "EILit" ->
            case D.decodeValue (D.field "value" D.int) payload of
                Ok i ->
                    EILit i

                Err err ->
                    EError (Debug.toString err)

        Ok "EHTML" ->
            case D.decodeValue (D.field "value" D.string) payload of
                Ok s ->
                    EHTML s

                Err err ->
                    EError (Debug.toString err)

        Ok "ESLit" ->
            case D.decodeValue (D.field "value" D.string) payload of
                Ok s ->
                    ESLit s

                Err err ->
                    EError (Debug.toString err)

        Ok "EImage" ->
            case D.decodeValue (D.field "value" D.string) payload of
                Ok i ->
                    EImage i

                Err err ->
                    EError (Debug.toString err)

        Ok "EJumpLink" ->
            case D.decodeValue (D.field "value" (D.map2 Tuple.pair (D.index 0 D.string) (D.index 1 D.string))) payload of
                Ok ( label, str ) ->
                    EJumpLink label str

                Err err ->
                    EError (Debug.toString err)

        Ok "EVideo" ->
            case D.decodeValue (D.field "value" D.string) payload of
                Ok v ->
                    EVideo v

                Err err ->
                    EError (Debug.toString err)

        Ok "EAudio" ->
            case D.decodeValue (D.field "value" D.string) payload of
                Ok v ->
                    EAudio v

                Err err ->
                    EError (Debug.toString err)

        Ok "EEmpty" ->
            EEmpty

        _ ->
            EError ("COMET value not implemented" ++ Debug.toString valueType)



-- This is where we get super-detailed about rendering of our widgets


viewCell : Spreadsheet -> Maybe Cell -> Html msg
viewCell model res =
    case res of
        Nothing ->
            span [] [ text "" ]

        Just cell ->
            case cell.value of
                EILit num ->
                    span [] [ text (String.fromInt num) ]

                ESLit str ->
                    span [] [ text str ]

                EHref str ->
                    a [ href str ] [ text str ]

                EBot ->
                    span [] [ text "()" ]

                --ESuperFancyGraph g ->
                --    let
                --        l1 = Graph.nodes g |> List.length
                --        l2 = Graph.edges g |> List.length
                --        textValue2 = "Graph of size " ++ (fromInt l1) ++ ", with " ++ (fromInt l2) ++ " edges"
                --        textValue = toGraphviz g
                --    in
                --        span [ class "vizjs-compile-dot-to-svg" ] [ text textValue ]
                --ECellGraph g ->
                --    span [ class "vizjs-compile-dot-to-svg" ] [ text (g |> mapNodes (\cellNode -> cellNode.value |> Debug.toString) |> Graph.DOT.output Just (always Nothing)) ]
                EJumpLink label src ->
                    span [] [ a [ href src ] [ text label ] ]

                EImage imgSrc ->
                    --span [] [ text ("<img src=" ++ src ++">") ]
                    span [] [ img [ src imgSrc ] [] ]

                EApp f args ->
                    let
                        resultOfEvaluation =
                            eval model cell.value
                    in
                    viewCell model (Just { cell | value = resultOfEvaluation })

                ECellRef addr ->
                    let
                        resultOfEvaluation =
                            eval model cell.value
                    in
                    viewCell model (Just { cell | value = resultOfEvaluation })

                EComet cometKey ->
                    case Dict.get cometKey model.cometStorage of
                        Just ( fx, val ) ->
                            viewCell model (Just { cell | value = val, formula = fx })

                        _ ->
                            span [] [ text ("comet pending.." ++ cometKey) ]

                EEmpty ->
                    span [] [ text ". " ]

                EAudio url ->
                    audioElemForUrl model url

                v ->
                    span [] [ text (Debug.toString v) ]



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
        D.map
            (always msg)
            (keyCode |> D.andThen (is_ 13))



-- It seems like we don't need this one anymore? (see [Keypress Down])


onArrowDown msg =
    on "keydown" <|
        D.map
            (always msg)
            (keyCode |> D.andThen (is_ 40))


is_ : Int -> Int -> D.Decoder ()
is_ target code =
    if code == target then
        D.succeed ()

    else
        D.fail "not the right key code"


viewCellInEditMode addr res =
    case res of
        Just cell ->
            input
                [ value cell.buffer
                , onInput (UpdateCellBuffer addr)
                , onBlur (Save addr)
                , autofocus True
                , id cssKeyForEditCellInput
                , class "widget-edit-mode-cell-input"
                ]
                []

        _ ->
            viewCellInEditMode addr (Just emptyCell)



--cellBelongsToAGraph addr expr = case expr of case List.find (\v -> v.addr == addr) nodes.graph of
--    Just v -> True
--    Nothing -> False
-- [note]:
-- If the cell under view is a Graph
-- we want to highlight cells in the spreadsheet that belong to the graph.
-- (This is only useful when we can successfuly extract graphs from the spreadsheet)


computeCellSelectionClass model addr =
    let
        ( rho, kappa ) =
            addr

        modelClasses =
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
                    if addrInVertexDemo addr model.demoVertices then
                        "elm-cell-part-of-primary-demonstration"

                    else
                        ""

                EdgeDemoMode1 ->
                    if addrInVertexDemo addr model.demoVertices then
                        "elm-cell-part-of-secondary-demonstration"

                    else
                        ""

                EdgeDemoMode2 cellVertex ->
                    if addrInVertexDemo addr model.demoVertices || addrInVertexDemo addr [ cellVertex ] then
                        "elm-cell-part-of-secondary-demonstration"

                    else
                        ""

                _ ->
                    ""

        addressClasses =
            "row-" ++ Debug.toString rho ++ " " ++ "column-" ++ Debug.toString kappa
    in
    modelClasses ++ " " ++ addressClasses


fileBeingDroppedOn : Mode -> CellAddress -> Bool
fileBeingDroppedOn mode addr =
    case mode of
        FileDropMode _ modeAddr ->
            modeAddr == addr

        _ ->
            False


oneCell : CellAddress -> Model -> Html Msg
oneCell addr model =
    if model.mode == EditMode addr then
        td [] [ viewCellInEditMode addr (find (\x -> x.addr == addr) model.database) ]

    else if model.mode == VertexDemoMode then
        td
            [ onClick (CollectVertexDemo addr)
            , class (computeCellSelectionClass model addr)
            ]
            [ viewCell model (find (\x -> x.addr == addr) model.database) ]

    else if model.mode == EdgeDemoMode1 then
        td
            [ onClick (CollectEdgeDemo1 addr)
            , class (computeCellSelectionClass model addr)
            ]
            [ viewCell model (find (\x -> x.addr == addr) model.database) ]

    else
        case model.mode of
            EdgeDemoMode2 cellVertex ->
                td
                    [ onClick (CollectEdgeDemo2 cellVertex addr)
                    , class (computeCellSelectionClass model addr)
                    ]
                    [ viewCell model (find (\x -> x.addr == addr) model.database) ]

            _ ->
                let
                    clickActions =
                        case model.mode of
                            RegisterFlushMode expr ->
                                [ onClick (FlushRegister addr expr) ]

                            _ ->
                                [ onDoubleClick (EditIntent addr Nothing), onClick (SwitchToMode (IdleMode addr)) ]

                    dragDropAttributes =
                        [ hijackOn "dragenter" (D.succeed (DragEnter addr))
                        , hijackOn "dragover" (D.succeed (DragEnter addr))
                        , hijackOn "dragleave" (D.succeed DragLeave)
                        , hijackOn "drop" (dropDecoder addr)
                        ]

                    fileDropCSS =
                        if fileBeingDroppedOn model.mode addr then
                            [ class "elm-active-drop-zone" ]

                        else
                            []
                in
                td ([ class (computeCellSelectionClass model addr) ] ++ clickActions ++ dragDropAttributes ++ fileDropCSS)
                    [ viewCell model (find (\x -> x.addr == addr) model.database) ]


viewRow : Int -> Model -> List (Html Msg)
viewRow rho model =
    let
        classForHeaderColumn =
            case model.mode of
                IdleMode ( idleRho, _ ) ->
                    if idleRho == rho then
                        [ class "elm-selected-row" ]

                    else
                        [ class "header-column" ]

                _ ->
                    [ class "header-column" ]
    in
    [ tr []
        [ td classForHeaderColumn [ text (rho + 1 |> String.fromInt) ]
        , oneCell ( rho, 0 ) model
        , oneCell ( rho, 1 ) model
        , oneCell ( rho, 2 ) model
        , oneCell ( rho, 3 ) model
        , oneCell ( rho, 4 ) model
        , oneCell ( rho, 5 ) model
        , oneCell ( rho, 6 ) model
        , oneCell ( rho, 7 ) model
        ]
    ]


viewRows : Model -> List (Html Msg)
viewRows model =
    List.range 0 100 |> List.map (\i -> viewRow i model) |> List.concat


calculateClassForRow rowLabel model =
    case model.mode of
        IdleMode ( rho, kappa ) ->
            if elemIndex rowLabel [ "A", "B", "C", "D", "E", "F", "G", "H", "I" ] == Just kappa then
                class "elm-selected-column"

            else
                class ""

        _ ->
            class ""


topRow model =
    tr [ class "top-row" ]
        [ td [] [ text " " ]
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
            span [] [ text "👁 Try navigating to a new cell by using arrow keys. Press enter or start typing to edit a cell" ]

        EditMode _ ->
            span [] [ text "👁 Try typing in a formula like (=1+1)" ]

        VertexDemoMode ->
            span [] [ text "👁 Show me an example of a few nodes, then click on \"Demonstrate Edges\" once you are done. :)" ]

        EdgeDemoMode1 ->
            let
                message =
                    case List.length model.demoEdges of
                        0 ->
                            "Start providing a demonstration of an edge by clicking on a source vertex"

                        1 ->
                            "Provide more edges, or click _Done_"

                        _ ->
                            "If you are happy with the provided demonstration, press _generalize_"
            in
            span [] [ text message ]

        EdgeDemoMode2 _ ->
            span [] [ text "... click on the second vertex to finalize edge demonstration" ]

        RegisterFlushMode _ ->
            span [] [ text "click on the desired cell to replace its content" ]

        FileDropMode _ addr ->
            span [] [ "Drop the file on " ++ Debug.toString addr |> text ]



-- _ -> span [ ] [ text "I'm not trained to assist you in this mode :(" ]


notImplementedButtons =
    div [ id "piet-not-implemented", class "piet-button-row" ]
        [ -- , hr [] []
          -- , button [  ]  [ text "dot notation for nested records" ]
          button [] [ text "gridlet repo" ]
        , button [] [ text "spill and shift" ]
        , button [] [ text "formula adjustment algorithm" ]
        , hr [] []
        , button [] [ text "create a patch for an external git repository 🐙" ]
        , hr [] []
        , button [] [ text "create a new website" ]
        , button [] [ text "preview on web" ]
        , button [] [ text "publish to web" ]

        -- , hr [] []
        -- , button [  ]  [ text "pause ⏸" ]
        , hr [] []
        , button [] [ text "[shift + enter] multiline editing a cell (for HTML)" ]
        , button [] [ text "[load via formulas] side-by-side n-way hypertext documents" ]
        ]


loadExampleButtons =
    div [ id "container-examples" ]
        [ button [ onClick (SwitchSpreadsheet exampleSpreadsheetLegend) ] [ text "Example 0: Types" ]
        , button [ onClick (SwitchSpreadsheet exampleSpreadsheetTheCities) ]
            [ text "Example 1: The Cities" ]
        , button [ onClick (SwitchSpreadsheet exampleSpreadsheet) ]
            [ text "Example 2: Matrix" ]
        , button [ onClick (SwitchSpreadsheet exampleSpreadsheetWithGraph) ]
            [ text "Example 3: Matrix with Graph" ]
        , button [ onClick (SwitchSpreadsheet exampleSpreadsheetAdjacencyListWithGraph) ]
            [ text "Example 4: Adjacency list with Graph" ]

        --, button [ onClick (SwitchSpreadsheet exampleSpreadsheetRemote) ]
        --[ text "Example 5: Remote search" ]
        , button [ onClick (SwitchSpreadsheet exampleSpreadsheetJSON) ]
            [ text "Example 6: Organization Chart (JSON)" ]
        , button [ onClick (SwitchSpreadsheet exampleSpreadsheetPoppet) ]
            [ text "Example 7: Poppet" ]
        ]


vertexDemoButtons model =
    div [ id "container-demo-buttons" ]
        [ button [ id "magic-button-demo-vertex", onClick (SwitchToMode VertexDemoMode) ]
            [ text ("demonstrate vertices (" ++ (model.demoVertices |> List.length |> Debug.toString) ++ ")") ]
        , button [ id "magic-button-demo-edge", onClick (SwitchToMode EdgeDemoMode1) ]
            --[ text "demonstrate edges" ]
            [ text ("demonstrate edges (" ++ (model.demoEdges |> List.length |> Debug.toString) ++ ")") ]
        , button [ id "magic-button-generalize", onClick (IdleMode ( 0, 0 ) |> SwitchToMode) ] [ text "generalize" ]
        ]


graphExtractionButtons model =
    div [ id "container-demo-buttons" ]
        [ button [ onClick ExtractGraphFromIncidenceMatrix ] [ text "extract a weighted graph from incidence matrix" ]
        , button [] [ text "extract graph from adjacency list" ]
        ]


spreadsheetInterface model =
    div [ id "container-spreadsheet" ]
        [ table [ class "spreadsheet" ] ([ topRow model ] ++ viewRows model)
        ]


mondrian =
    img [ src mondrianSrc ] []


audioElemForUrl model url =
    div []
        [ audio
            [ src url

            --, on "timeupdate" (Json.map CurrentTime targetCurrentTime)
            --, on "seek" (Json.map CurrentTime targetCurrentTime)
            --, on "seek" (Json.succeed Seeking)
            --, on "seeking" (Json.succeed Seeking)
            --, on "seekend" (Json.succeed Paused)
            --, on "playing" (Json.succeed Playing)
            --, on "play" (Json.succeed Playing)
            --, on "pause" (Json.succeed Playing)
            --, on "ended" (Json.succeed Paused)
            --, on "loadedmetadata" (Json.succeed Paused)
            , controls True
            ]
            []

        --, div [] [text (toString model.currentTime)]
        --, div [] [text (toString model.videoState)]
        ]


videoElemForUrl model url =
    div []
        [ video
            [ src "https://www.sample-videos.com/video123/mp4/720/big_buck_bunny_720p_1mb.mp4"

            --, on "timeupdate" (Json.map CurrentTime targetCurrentTime)
            --, on "seek" (Json.map CurrentTime targetCurrentTime)
            --, on "seek" (Json.succeed Seeking)
            --, on "seeking" (Json.succeed Seeking)
            --, on "seekend" (Json.succeed Paused)
            --, on "playing" (Json.succeed Playing)
            --, on "play" (Json.succeed Playing)
            --, on "pause" (Json.succeed Playing)
            --, on "ended" (Json.succeed Paused)
            --, on "loadedmetadata" (Json.succeed Paused)
            , controls True
            ]
            []

        --, div [] [text (toString model.currentTime)]
        --, div [] [text (toString model.videoState)]
        ]


alternativeViewByEExpr : Model -> EExpr -> Html a
alternativeViewByEExpr model value =
    case value of
        EILit num ->
            text (Debug.toString num)

        EImage url ->
            img [ src url ] []

        EVideo url ->
            videoElemForUrl model url

        EAudio url ->
            audioElemForUrl model url

        ESuperFancyGraph g ->
            pre [] [ toGraphviz g |> text ]

        EYouTube params ->
            let
                startAndEnd =
                    case ( params.start, params.end ) of
                        ( Nothing, Nothing ) ->
                            ""

                        ( Nothing, Just x ) ->
                            "?start=0&end=" ++ x

                        ( Just x, Nothing ) ->
                            "?start=" ++ x

                        ( Just x, Just y ) ->
                            "?start=" ++ x ++ "&end=" ++ y
            in
            iframe
                [ width 560
                , height 315
                , src ("https://www.youtube.com/embed/" ++ params.video_id ++ startAndEnd)
                , property "frameborder" (E.string "0")
                , property "allowfullscreen" (E.string "true")
                ]
                []

        EComet cometKey ->
            case Dict.get cometKey model.cometStorage of
                Just ( fx, resolvedVal ) ->
                    span []
                        [ text ("Resolved comet value with key " ++ cometKey)
                        , hr [] []
                        , alternativeViewByEExpr model resolvedVal
                        ]

                Nothing ->
                    text ("Non-resolved comet value with key " ++ cometKey)

        ESLit str ->
            text str

        EHTML myHTML ->
            div [] (textHtml myHTML)

        _ ->
            code [] [ Debug.toString value |> text ]


textHtml : String -> List (Html.Html msg)
textHtml t =
    case Html.Parser.run t of
        Ok nodes ->
            Html.Parser.Util.toVirtualDom nodes

        Err _ ->
            []


sideviewRender : Model -> CellAddress -> Html a
sideviewRender model addr =
    case find (\x -> x.addr == addr) model.database of
        Just cell ->
            alternativeViewByEExpr model cell.value

        Nothing ->
            span [] []



--Nothing -> mondrian


sidePane : Model -> CellAddress -> Html a
sidePane model addr =
    table []
        [ tr [] [ td [] [ sideviewRender model ( -1, 0 ) ], td [] [ sideviewRender model ( -1, 1 ) ] ]
        , tr [] [ td [] [ sideviewRender model addr ] ]
        ]


alternativeViewInterface : Model -> Html a
alternativeViewInterface model =
    case model.mode of
        IdleMode addr ->
            sidePane model addr

        EditMode addr ->
            sidePane model addr

        -- _ -> span [] []
        _ ->
            mondrian



--table [ id "container-alternative-view" ] [
--    tr [] [
--        td [] [ text "Hello" ]
--    ,   td [] [ text "World!"]
--    ]
--]


onlineOrOffline =
    div [ class "container-row" ]
        [ text "😴 [offline mode]"
        , button [] [ text "your client id is #42" ]
        ]


highlightingBar =
    div [ class "piet-button-row" ]
        [ div []
            [ button [] [ text "Highlight A2:G10" ]
            , button [] [ text "👀" ]
            ]
        ]


containerHeader model =
    div [ id "container-header", class "container-row" ]
        [ div [] []

        --, div [ ] [ loadExampleButtons ]
        , div [ id "container-minicell-logo" ]
            [ img [ src "" ] [] -- img [ src "http://shiraz.local/~nima/wiki/nima/resources/assets/wiki.png" ] []
            , text "Minicell"
            , span [ class "minicell-version" ] [ text "(Version 0.0.5)" ]
            ]

        -- , div [ ] [ onlineOrOffline ]
        -- , div [ ] [ highlightingBar ]
        -- , div [ ] [ notImplementedButtons ]
        --, div [ ] [ vertexDemoButtons model ]
        --, div [ ] [ graphExtractionButtons model ]
        , div [] [ formulaBar model ]
        ]


formulaBarValue model =
    case model.mode of
        IdleMode addr ->
            case find (\x -> x.addr == addr) model.database of
                Just cell ->
                    cell.formula

                Nothing ->
                    "EMPTY CELL"

        EditMode addr ->
            case find (\x -> x.addr == addr) model.database of
                Just cell ->
                    case cell.buffer of
                        "" ->
                            cell.formula

                        _ ->
                            cell.buffer

                Nothing ->
                    "EMPTY CELL IN EDIT MODE"

        _ ->
            "What mode are you in, my friend?"


formulaBar model =
    div [ id "formula-bar" ]
        [ span [ id "widget-phi" ] [ text "fx" ]

        --  , img [ id "icon-formula-bar", src "/icons/formula-f.svg" ] []
        , span [ id "widget-formula-bar-input-container" ] [ input [ id "widget-formula-bar", value (formulaBarValue model) ] [] ]
        ]


pinnedViewInterface model =
    []



--[ tr [] [ td [] [ sideviewRender model (0,0) ] ]
--, tr [] [ td [] [ sideviewRender model (1,0) ] ]
--, tr [] [ td [] [ sideviewRender model (2,0) ] ]
--, tr [] [ td [] [ sideviewRender model (3,0) ] ]
--, tr [] [ td [] [ sideviewRender model (4,0) ] ]
--]


paneB : Model -> Html a
paneB model =
    table [] ([ tr [] [ td [] [ alternativeViewInterface model ] ] ] ++ pinnedViewInterface model)


containerPanes model =
    div [ id "container-panes" ]
        [ div [ id "pane-a" ] [ spreadsheetInterface model ]
        , div [ id "pane-b" ] [ paneB model ]
        ]


containerPanes2 model =
    div [ id "container-panes" ]
        [ table [ id "container-panes" ]
            [ tr []
                [ td [ id "pane-a" ] [ spreadsheetInterface model ]
                , td [ id "pane-b" ] [ paneB model ]
                ]
            ]
        ]


mainInterface model =
    div [ id "container-content", class "container-row" ]
        [ containerPanes model
        ]


footerContent model =
    div [ id "container-footer", class "container-row" ]
        [ table []
            [ tr []
                [ td [ id "clippy" ] [ clippy model ] -- Summon Clippy
                , td [] [ text (Debug.toString model.mode) ]
                , td [] [ text (Debug.toString model.location) ]
                , td [] [ text (Debug.toString (minicellEndpoint model EPAll)) ]

                -- , td [ ] [ text (Debug.toString model.mouseInfo)]
                --, tr [] [ td [] [ text (Debug.toString model.cometStorage) ] ]
                ]
            ]
        ]


a0 model =
    table [ class "spreadsheet a0" ] ([ topRow model ] ++ viewRow -1 model)



-- viewRow 0 model -- div [ id "A0" ] [text "A0"]


view : Model -> Html Msg
view model =
    div [ id "container-box" ]
        ([ containerHeader model
         , mainInterface model
         , a0 model
         ]
            ++ [ footer []
                    [ footerContent model
                    ]
               ]
         --++ (debugView model)
        )


debugView model =
    [ code []
        [ text "mode: "
        , text (Debug.toString model.mode)
        , hr [] []
        , text (Debug.toString model)
        ]
    ]


mainSimple =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


main =
    Browser.application
        { init = initWithLocation
        , update = update
        , view = \model -> { title = "Piet", body = [ view model ] }
        , subscriptions = subscriptions
        , onUrlChange = \_ -> NoOp
        , onUrlRequest = \_ -> NoOp
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ keyPress WindowKeyPress
        , Time.every 500 Tick
        ]



--viewPreview : String -> Html msg
--viewPreview url =
--  div
--    [ style "width" "60px"
--    , style "height" "60px"
--    , style "background-image" ("url('" ++ url ++ "')")
--    , style "background-position" "center"
--    , style "background-repeat" "no-repeat"
--    , style "background-size" "contain"
--    ]
--    []


dropDecoder : CellAddress -> D.Decoder Msg
dropDecoder addr =
    D.at [ "dataTransfer", "files" ] (D.oneOrMore (GotFiles addr) File.decoder)


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (D.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )


cometUpdate : Model -> CometKey -> Cmd Msg
cometUpdate model cometKey =
    Http.get
        { url = minicellEndpoint model (EPShow cometKey)
        , expect = Http.expectJson (CometUpdate cometKey) D.value
        }


cometUpdateAll : Model -> Cmd Msg
cometUpdateAll model =
    Http.get
        { url = minicellEndpoint model EPAll
        , expect = Http.expectJson CometUpdateAll D.value
        }


toGraphviz g =
    g
        |> mapNodes
            (\( _, listOfCells ) ->
                List.head listOfCells
                    |> Maybe.withDefault emptyCell
                    |> Debug.toString
            )
        |> Graph.DOT.output Just (always Nothing)
