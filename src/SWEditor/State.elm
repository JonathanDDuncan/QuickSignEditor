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
import SWEditor.Undo exposing (..)
import SW.Types exposing (..)
import Mouse as Mouse exposing (downs, moves, ups)
import Keyboard.Shared exposing (..)
import List.Extra exposing (..)
import SWEditor.SignArea exposing (..)


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
                { model | sign = editorSign, uid = lastuid }
                    ! []
                    |> Update.Extra.andThen update UpdateSignViewPosition

        RequestElementPosition elementid ->
            ( model, Ports.requestElementPosition elementid )

        ReceiveElementPosition namedposition ->
            { model | viewposition = namedposition }
                ! []
                |> Update.Extra.andThen update CenterSign

        UpdateSignViewPosition ->
            { model | windowresized = False }
                ! []
                |> Update.Extra.andThen update (RequestElementPosition "signView")

        CenterSign ->
            model
                ! []
                |> Update.Extra.andThen update (AddUndo True "CenterSign" { model | sign = centerSignViewposition model.viewposition model.sign })

        StartRectangleSelect ->
            { model | editormode = RectangleSelect, rectanglestart = model.xy } ! []

        EndRectangleSelect ->
            let
                newsign =
                    rectangleselect model

                changed =
                    symbolshavechanged model.sign.syms newsign.syms
            in
                model
                    ! []
                    |> Update.Extra.andThen update (AddUndo changed "EndRectangleSelect" { model | editormode = Awaiting, sign = newsign })

        SelectSymbol id ->
            let
                newsign =
                    selectSymbolId id model

                changed =
                    symbolshavechanged model.sign.syms newsign.syms
            in
                model
                    ! []
                    |> Update.Extra.andThen update (AddUndo changed "SelectSymbol" { model | sign = newsign })
                    |> Update.Extra.andThen update (StartDragging)

        UnSelectSymbols ->
            let
                newsign =
                    unselectSymbols model.sign

                changed =
                    symbolshavechanged model.sign.syms newsign.syms
            in
                model
                    ! []
                    |> Update.Extra.andThen update (AddUndo changed "UnSelectSymbols" { model | sign = newsign })

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
                    |> Update.Extra.filter (model.editormode == Dragging || model.editormode == AddingSymbol)
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
                    |> Update.Extra.filter (model.editormode == Dragging || model.editormode == AddingSymbol)
                        (Update.Extra.andThen update DragSelected)

        ReplaceSymbol symbol ->
            let
                selectedsymbols =
                    countselectedsymbols model.sign.syms

                syms =
                    if selectedsymbols == 1 then
                        replaceselectedsymbols model.sign.syms symbol
                    else
                        model.sign.syms

                sign1 =
                    model.sign

                sign =
                    { sign1
                        | syms = syms
                    }
            in
                { model
                    | sign = sign
                }
                    ! []

        StartDragging ->
            { model
                | editormode = Dragging
                , dragstart = model.xy
                , dragsign = model.sign
            }
                ! []

        DragSelected ->
            { model
                | sign =
                    Drag.dragsign model
            }
                ! []

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
                        | syms =
                            List.append sign1.syms
                                [ { selectedsymbol
                                    | x = model.xy.x
                                    , y = model.xy.y
                                    , id = model.uid + 2
                                  }
                                ]
                    }

                lastuid =
                    getlastuid <| sign
            in
                { model
                    | uid = lastuid
                    , editormode = AddingSymbol
                    , dragstart = model.xy
                    , dragsign = sign
                }
                    ! []

        EndDragging ->
            let
                undroppedsymbol =
                    getundroppedsymbol model.editormode model.sign.syms

                iswithinview =
                    issymbolwithinview model.viewposition undroppedsymbol

                newmodel =
                    if not iswithinview then
                        deletesymbols model
                    else
                        model

                signwithinbounds =
                    putsymbolswithinbounds newmodel.sign newmodel.viewposition
            in
                newmodel
                    ! []
                    |> Update.Extra.andThen update
                        (AddUndo True
                            "EndDragging"
                            { newmodel
                                | sign = signwithinbounds
                                , editormode = Awaiting
                            }
                        )

        AddUndo changed actiononame model1 ->
            addUndo changed actiononame model1 ! []

        Undo ->
            undo model ! []

        Redo ->
            redo model ! []

        DeleteSymbols ->
            model
                ! []
                |> Update.Extra.andThen update
                    (AddUndo True "DeleteSymbols" <| deletesymbols model)

        Keyboard command ->
            runKeyboardCommand model command

        MoveSymbols direction distance ->
            model
                ! []
                |> Update.Extra.andThen update
                    (AddUndo True "MoveSymbols" <| movesymbols model direction distance)


replaceselectedsymbols : List EditorSymbol -> Symbol -> List EditorSymbol
replaceselectedsymbols syms symbol =
    List.map (\sym -> replaceselected sym symbol) syms


replaceselected : EditorSymbol -> Symbol -> EditorSymbol
replaceselected sym symbol =
    if not sym.selected then
        sym
    else
        { sym
            | width = symbol.width
            , height = symbol.height
            , fontsize = symbol.fontsize
            , nwcolor = symbol.nwcolor
            , pua = symbol.pua
            , code = symbol.code
            , key = symbol.key
            , nbcolor = symbol.nbcolor
        }


symbolshavechanged : List a -> List a -> Bool
symbolshavechanged firstsymbols secondsymbols =
    not <| List.Extra.isPermutationOf firstsymbols secondsymbols


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
    model
        ! []
        |> Update.Extra.filter (command.ctrlPressed && List.any ((==) 43) command.keys)
            (Update.Extra.andThen update Undo)
        |> Update.Extra.filter (command.ctrlPressed && List.any ((==) 21) command.keys)
            (Update.Extra.andThen update Redo)
        |> Update.Extra.filter (List.any ((==) 62) command.keys)
            (Update.Extra.andThen update DeleteSymbols)
        |> Update.Extra.filter (List.any ((==) 67) command.keys)
            (Update.Extra.andThen update <| MoveSymbols Up 1)
        |> Update.Extra.filter (List.any ((==) 69) command.keys)
            (Update.Extra.andThen update <| MoveSymbols Down 1)
        |> Update.Extra.filter (List.any ((==) 70) command.keys)
            (Update.Extra.andThen update <| MoveSymbols Right 1)
        |> Update.Extra.filter (List.any ((==) 68) command.keys)
            (Update.Extra.andThen update <| MoveSymbols Left 1)
        |> Update.Extra.filter (command.ctrlPressed && List.any ((==) 67) command.keys)
            (Update.Extra.andThen update <| MoveSymbols Up 10)
        |> Update.Extra.filter (command.ctrlPressed && List.any ((==) 69) command.keys)
            (Update.Extra.andThen update <| MoveSymbols Down 10)
        |> Update.Extra.filter (command.ctrlPressed && List.any ((==) 70) command.keys)
            (Update.Extra.andThen update <| MoveSymbols Right 10)
        |> Update.Extra.filter (command.ctrlPressed && List.any ((==) 68) command.keys)
            (Update.Extra.andThen update <| MoveSymbols Left 10)


runKeyboardGeneralChooser model command =
    model ! []


runKeyboardGroupChooser model command =
    model ! []


runKeyboardSymbolChooser model command =
    model ! []



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
        , subReplaceSymbol ReplaceSymbol
        , receiveKeyboardCommand Keyboard
        ]



-- To nest subscriptions
-- Sub.batch
--       [ SubSWEditor.State.subscriptions model.subSWEditorFieldName |> Sub.map SubSWEditorMsg
--       ]
