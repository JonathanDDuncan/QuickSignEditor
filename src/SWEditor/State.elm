module SWEditor.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import Ports as Ports exposing (..)
import Debug
import Update.Extra exposing (..)
import SWEditor.Types exposing (..)
import SWEditor.RectangleSelect exposing (..)
import SWEditor.Drag as Drag exposing (..)
import SWEditor.Select exposing (..)
import SWEditor.EditorSign exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SW.Types exposing (..)
import List.Extra exposing (..)
import Mouse as Mouse exposing (downs, moves, ups)
import Keyboard.Shared exposing (..)


-- import SubSWEditors.State


init : ( Model, Cmd Msg )
init =
    ( { fsw = "M518x533S1870a489x515S18701482x490S20500508x496S2e734500x468"
      , sign = signinit
      , xy = Position 0 0
      , dragstart = Position 0 0
      , dragsign = signinit
      , rectanglestart = Position 0 0
      , windowresized = False
      , editormode = Awaiting
      , viewposition = { name = "", x = 0, y = 0, width = 0, height = 0 }
      , containerheight = 500
      , uid = 0
      , undolist = []
      , redolist = []
      }
    , Cmd.none
    )


update : SWEditor.Types.Msg -> SWEditor.Types.Model -> ( SWEditor.Types.Model, Cmd SWEditor.Types.Msg )
update action model =
    case action of
        ChangeFSW newfsw ->
            { model | fsw = newfsw, sign = signinit } ! []

        RequestSign ->
            ( model, Ports.requestSign model.fsw )

        RequestSignfromOtherApp ->
            ( model, Ports.requestSignfromOtherApp "" )

        SetSign newsign ->
            let
                editorSign =
                    toEditorSign newsign model.uid

                lastuid =
                    getlastuid <| editorSign
            in
                { model | sign = editorSign, uid = lastuid, undolist = addundoitem model "SetSign" } ! [] |> Update.Extra.andThen update UpdateSignViewPosition

        RequestElementPosition elementid ->
            ( model, Ports.requestElementPosition elementid )

        ReceiveElementPosition namedposition ->
            { model | viewposition = namedposition } ! [] |> Update.Extra.andThen update CenterSign

        UpdateSignViewPosition ->
            { model | windowresized = False } ! [] |> Update.Extra.andThen update (RequestElementPosition "signView")

        CenterSign ->
            { model | sign = centerSignViewposition model.viewposition model.sign, undolist = addundoitem model "CenterSign" } ! []

        StartDragging ->
            { model | editormode = Dragging, dragstart = model.xy, dragsign = model.sign } ! []

        DragSelected ->
            { model
                | sign =
                    Drag.dragsign model
                    -- , undolist = addundoitem model "DragSelected"
            }
                ! []

        EndDragging ->
            let
                signwithinbounds =
                    putsymbolswithinbounds model.sign model.viewposition
            in
                { model | sign = signwithinbounds, editormode = Awaiting, undolist = addundoitem model "EndDragging" } ! []

        StartRectangleSelect ->
            { model | editormode = RectangleSelect, rectanglestart = model.xy } ! []

        EndRectangleSelect ->
            { model | editormode = Awaiting, sign = rectangleselect model, undolist = addundoitem model "EndRectangleSelect" } ! []

        SelectSymbol id ->
            let
                newsign =
                    selectSymbolId id model

                undolist1 =
                    if (Debug.log "not same" <| newsign /= model.sign) then
                        addundoitem model "SelectSymbol"
                    else
                        model.undolist
            in
                { model | sign = newsign, undolist = undolist1 } ! [] |> Update.Extra.andThen update (StartDragging)

        UnSelectSymbols ->
            { model | sign = unselectSymbols model.sign, undolist = addundoitem model "UnSelectSymbols" } ! []

        MouseDown position ->
            let
                signviewposition =
                    (signViewPosition position model.viewposition)

                withinsignview =
                    (withinSignView signviewposition model.viewposition)

                symbolsunderposition =
                    (symbolsUnderPosition signviewposition model.sign.syms)

                howmanysymbolsunderposition =
                    List.length symbolsunderposition

                selectedsymbols =
                    countselectedsymbols symbolsunderposition

                firstsymbol =
                    List.head symbolsunderposition

                firstsymbolid =
                    symbolId firstsymbol
            in
                model
                    ! []
                    |> Update.Extra.filter (selectedsymbols > 0 && withinsignview)
                        (Update.Extra.andThen update (StartDragging))
                    |> Update.Extra.filter (howmanysymbolsunderposition == 1 && withinsignview)
                        (Update.Extra.andThen update (SelectSymbol firstsymbolid))
                    |> Update.Extra.filter (howmanysymbolsunderposition == 0 && withinsignview)
                        (Update.Extra.andThen update (UnSelectSymbols)
                            >> Update.Extra.andThen update StartRectangleSelect
                        )

        MouseUp position ->
            let
                signviewposition =
                    (signViewPosition position model.viewposition)

                withinsignview =
                    (withinSignView signviewposition model.viewposition)

                symbolsunderposition =
                    (symbolsUnderPosition signviewposition model.sign.syms)
            in
                model
                    ! []
                    |> Update.Extra.filter (model.editormode == RectangleSelect)
                        (Update.Extra.andThen update EndRectangleSelect)
                    |> Update.Extra.filter (model.editormode == Dragging)
                        (Update.Extra.andThen update EndDragging)

        MouseMove position ->
            let
                signviewposition =
                    signViewPosition position model.viewposition

                withinsignview =
                    withinSignView signviewposition model.viewposition

                symbolsunderposition =
                    symbolsUnderPosition signviewposition model.sign.syms
            in
                { model | xy = signviewposition }
                    ! []
                    |> Update.Extra.filter (model.windowresized)
                        (Update.Extra.andThen update UpdateSignViewPosition)
                    |> Update.Extra.filter (model.editormode == Dragging)
                        (Update.Extra.andThen update DragSelected)

        DragSymbol symbol ->
            let
                editorSymbol =
                    toEditorSymbol model.uid 0 symbol

                selectedsymbol =
                    { editorSymbol | selected = True }

                sign1 =
                    unselectSymbols model.sign

                sign =
                    { sign1
                        | syms = List.append sign1.syms [ { selectedsymbol | x = model.xy.x, y = model.xy.y, id = model.uid + 2 } ]
                    }

                lastuid =
                    getlastuid <| sign
            in
                { model | uid = lastuid, editormode = Dragging, dragstart = model.xy, dragsign = sign } ! []

        Undo ->
            undo model ! []

        Redo ->
            redo model ! []

        Keyboard command ->
            let
                a =
                    Debug.log "Keyboard command" command

                updatetuple =
                    runKeyboardCommand model command
            in
                updatetuple


runKeyboardCommand model command =
    let
        mode =
            getKeyboardMode command.mode

        updatetuple =
            case mode of
                SignView ->
                    runKeyboardSignView model command

                GeneralChooser ->
                    runKeyboardGeneralChooser model command

                GroupChooser ->
                    runKeyboardGroupChooser model command

                SymbolChooser ->
                    runKeyboardSymbolChooser model command
    in
        updatetuple


runKeyboardSignView model command =
    let
        ud =
            Debug.log "Undo" command.ctrlPressed && List.any ((==) 43) command.keys
    in
        model
            ! []
            |> Update.Extra.filter (command.ctrlPressed && List.any ((==) 43) command.keys)
                (Update.Extra.andThen update Undo)
            |> Update.Extra.filter (command.ctrlPressed && List.any ((==) 21) command.keys)
                (Update.Extra.andThen update Redo)


runKeyboardGeneralChooser model command =
    model ! []


runKeyboardGroupChooser model command =
    model ! []


runKeyboardSymbolChooser model command =
    model ! []


addundoitem : Model -> String -> List UndoItem
addundoitem model actionname =
    (List.append model.undolist [ { actionname = actionname, sign = model.sign } ])


undo : Model -> Model
undo model =
    let
        undoitem1 =
            (Maybe.withDefault { actionname = "", sign = model.sign }) <| List.Extra.last model.undolist

        lastsign =
            undoitem1.sign

        len =
            List.length model.undolist

        length =
            if len >= 0 then
                len
            else
                1

        undolist =
            List.take (length - 1) model.undolist

        redolist =
            List.append model.redolist []

        actionname =
            Debug.log "Undo actionname" undoitem1.actionname
    in
        { model | sign = lastsign, undolist = undolist, redolist = redolist }


redo : Model -> Model
redo model =
    let
        redoitem1 =
            List.Extra.last model.redolist

        model =
            case redoitem1 of
                Just item ->
                    let
                        sign =
                            item.sign

                        len =
                            List.length model.redolist

                        length =
                            if len >= 0 then
                                len
                            else
                                1

                        undolist =
                            List.append model.undolist [ {actionname = "Redo", sign = model.sign } ]

                        redolist =
                            List.take (length - 1) model.redolist

                        actionname =
                            Debug.log "Undo actionname" item.actionname
                    in
                        { model | sign = sign, undolist = undolist, redolist = redolist }

                Nothing ->
                    model
    in
        model


putsymbolswithinbounds sign bounds =
    let
        movedsyms =
            List.map (\sym -> maintainwithinbounds sym <| bounds) sign.syms

        signbound =
            getSignBounding movedsyms
    in
        { sign | syms = movedsyms, width = signbound.width, height = signbound.height, x = signbound.x, y = signbound.y }


maintainwithinbounds sym bounds =
    let
        left =
            0

        right =
            0 + bounds.width - 20

        top =
            5

        bottom =
            0 + bounds.height - 10

        newx =
            if (sym.x < left) then
                left
            else if sym.x + sym.width > right then
                right - sym.width
            else
                sym.x

        newy =
            if (sym.y < top) then
                top
            else if sym.y + sym.height > bottom then
                bottom - sym.height
            else
                sym.y
    in
        { sym | x = newx, y = newy }



-- { model | editormode = Dragging, dragstart = model.xy, dragsign = model.sign } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.downs MouseDown
        , Mouse.moves MouseMove
        , Mouse.ups MouseUp
        , receiveSign SetSign
        , receiveElementPosition ReceiveElementPosition
        , subDragSymbol DragSymbol
        , receiveKeyboardCommand Keyboard
        ]



-- To nest subscriptions
-- Sub.batch
--       [ SubSWEditor.State.subscriptions model.subSWEditorFieldName |> Sub.map SubSWEditorMsg
--       ]
