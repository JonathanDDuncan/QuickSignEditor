module SWEditor.State exposing (init, update, subscriptions)

import Ports
    exposing
        ( requestSign
        , requestSignfromOtherApp
        , requestElementPosition
        , sendKeyboardMode
        , subaddsigntosignview
        , receiveElementPosition
        , subDragSymbol
        , subAddSymbol
        , subReplaceSymbol
        , receiveKeyboardCommand
        )
import Update.Extra exposing (updateModel)
import SWEditor.Types
    exposing
        ( Model
        , Msg(..)
        , EditorMode(..)
        , signViewPosition
        , withinSignView
        )
import SWEditor.RectangleSelect exposing (rectangleselect)
import SWEditor.Drag as Drag
import SWEditor.Select exposing (selectSymbolId, unselectSignSymbols)
import SWEditor.EditorSign exposing (updateSymbolIds, centerSignViewposition)
import SWEditor.EditorSymbol exposing (symbolsUnderPosition, countselectedsymbols, symbolId)
import SWEditor.Undo exposing (addUndo, undo, redo)
import SW.Types exposing (Position)
import SW.Sign exposing (signinit, lastsignid)
import SW.PortableSign exposing (portableSigntoSign)
import SW.Symbol exposing (Symbol)
import Mouse as Mouse
import List.Extra
import SWEditor.SignArea
    exposing
        ( getundroppedsymbol
        , issymbolwithinview
        , deletesymbols
        , putsymbolswithinbounds
        , duplicatesymbols
        , changesizesymbols
        , movesymbols
        )
import SWEditor.EditorKeyboard exposing (configKeyboardSignView, runKeyboardCommand)
import Keyboard.Shared exposing (KeyboardMode)
import SW.Identifier exposing (updateId)


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
      , signviewmargin = 5
      , uid = 0
      , undolist = []
      , redolist = []
      , signviewkeyboard = configKeyboardSignView
      }
    , Cmd.none
    )


update : SWEditor.Types.Msg -> SWEditor.Types.Model -> ( SWEditor.Types.Model, Cmd SWEditor.Types.Msg )
update action model =
    case action of
        ChangeFSW newfsw ->
            { model | fsw = newfsw, sign = signinit } ! []

        RequestSign ->
            ( model, requestSign model.fsw )

        RequestSignfromOtherApp ->
            ( model, requestSignfromOtherApp "" )

        SetSign portablesign ->
            let
                editorSign =
                    updateSymbolIds (portableSigntoSign portablesign) model.uid

                lastuid =
                    lastsignid <| editorSign
            in
                { model | sign = editorSign, uid = lastuid }
                    ! []
                    |> Update.Extra.andThen update UpdateSignViewPosition

        RequestElementPosition elementid ->
            ( model, requestElementPosition elementid )

        ReceiveElementPosition namedposition ->
            let
                adjustedformargin =
                    { namedposition | x = namedposition.x + model.signviewmargin, y = namedposition.y + model.signviewmargin }
            in
                { model | viewposition = adjustedformargin }
                    ! []
                    |> Update.Extra.andThen update CenterSign

        UpdateSignViewPosition ->
            { model | windowresized = False }
                ! []
                |> Update.Extra.andThen update (RequestElementPosition "signView")

        CenterSign ->
            model
                ! []
                |> updateModel
                    (\model1 ->
                        { model1 | sign = centerSignViewposition model.viewposition model.sign }
                    )
                |> addUndoEntry True "CenterSign"

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
                    |> updateModel
                        (\model1 ->
                            { model1 | editormode = Awaiting, sign = newsign }
                        )
                    |> addUndoEntry changed "EndRectangleSelect"

        SelectSymbol id ->
            let
                newsign =
                    selectSymbolId id model

                changed =
                    symbolshavechanged model.sign.syms newsign.syms
            in
                model
                    ! []
                    |> updateModel (\model1 -> { model1 | sign = newsign })
                    |> addUndoEntry changed "SelectSymbol"
                    |> Update.Extra.andThen update StartDragging

        UnSelectSymbols ->
            let
                newsign =
                    unselectSignSymbols model.sign

                changed =
                    symbolshavechanged model.sign.syms newsign.syms
            in
                model
                    ! []
                    |> updateModel (\model1 -> { model1 | sign = newsign })
                    |> addUndoEntry changed "UnSelectSymbols"

        MouseDown position ->
            let
                signviewposition =
                    signViewPosition position model.viewposition model.signviewmargin

                withinsignview =
                    withinSignView signviewposition model.viewposition

                symbolsunderposition =
                    symbolsUnderPosition signviewposition model.sign.syms

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
                        (Update.Extra.andThen update StartDragging)
                    |> Update.Extra.filter (howmanysymbolsunderposition == 1 && withinsignview)
                        (Update.Extra.andThen update (SelectSymbol firstsymbolid))
                    |> Update.Extra.filter (howmanysymbolsunderposition == 0 && withinsignview)
                        (Update.Extra.andThen update UnSelectSymbols
                            >> Update.Extra.andThen update StartRectangleSelect
                        )

        MouseUp position ->
            let
                signviewposition =
                    signViewPosition position model.viewposition model.signviewmargin
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
                    signViewPosition position model.viewposition model.signviewmargin

                symbolsunderposition =
                    symbolsUnderPosition signviewposition model.sign.syms
            in
                { model | xy = signviewposition }
                    ! []
                    |> Update.Extra.filter model.windowresized
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
                    updateId model.uid 0 symbol

                selectedsymbol =
                    { editorSymbol | selected = True }

                sign1 =
                    unselectSignSymbols model.sign

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
                    lastsignid <| sign
            in
                { model
                    | uid = lastuid
                    , editormode = AddingSymbol
                    , dragstart = model.xy
                    , dragsign = sign
                }
                    ! []

        AddSymbol symbol ->
            let
                editorSymbol =
                    updateId model.uid 0 symbol

                selectedsymbol =
                    { editorSymbol | selected = True }

                sign1 =
                    unselectSignSymbols model.sign

                sign =
                    { sign1
                        | syms =
                            List.append sign1.syms
                                [ { selectedsymbol
                                    | x = 200
                                    , y = 200
                                    , id = model.uid + 2
                                  }
                                ]
                    }

                lastuid =
                    lastsignid sign

                newmodel =
                    { model
                        | uid = lastuid
                        , editormode = Awaiting
                        , sign = sign
                    }
            in
                newmodel
                    ! []
                    |> addUndoEntry True "AddSymbol"

        EndDragging ->
            let
                undroppedsymbol =
                    getundroppedsymbol model.editormode model.sign.syms

                undroppediswithinview =
                    issymbolwithinview model.viewposition undroppedsymbol

                newmodel =
                    if not undroppediswithinview then
                        deletesymbols model
                    else
                        model

                signwithinbounds =
                    putsymbolswithinbounds newmodel.sign newmodel.viewposition
            in
                newmodel
                    ! []
                    |> updateModel
                        (\model1 ->
                            { model1
                                | sign = signwithinbounds
                                , editormode = Awaiting
                            }
                        )
                    |> addUndoEntry True "EndDragging"

        AddUndo changed actiononame model1 ->
            addUndo changed actiononame model1 ! []

        Undo ->
            undo model ! []

        Redo ->
            redo model ! []

        DeleteSymbols ->
            model
                ! []
                |> updateModel
                    (\model1 ->
                        deletesymbols model1
                    )
                |> addUndoEntry True "DeleteSymbols"

        DuplicateSymbols ->
            model
                ! []
                |> updateModel
                    (\model1 ->
                        duplicatesymbols model1
                    )
                |> addUndoEntry True "DuplicateSymbols"

        SizeIncreaseSymbols ->
            model
                ! []
                |> updateModel
                    (\model1 ->
                        changesizesymbols model1 0.1
                    )
                |> addUndoEntry True "SizeIncreaseSymbols"

        SizeDecreaseSymbols ->
            model
                ! []
                |> updateModel
                    (\model1 ->
                        changesizesymbols model1 -0.1
                    )
                |> addUndoEntry True "SizeDecreaseSymbols"

        Keyboard command ->
            runKeyboardCommand model command update

        MoveSymbols direction distance ->
            model
                ! []
                |> updateModel
                    (\model1 ->
                        movesymbols model1 direction distance
                    )
                |> addUndoEntry True "MoveSymbols"

        SetKeyboardMode mode ->
            let
                num =
                    Keyboard.Shared.getKeyboardModeCode mode
            in
                ( model
                , sendKeyboardMode num
                )


addUndoEntry :
    Bool
    -> String
    -> ( Model, Cmd Msg )
    -> ( Model, Cmd Msg )
addUndoEntry changed name updatetuple =
    updatetuple
        |> \(( model, _ ) as updatetuple1) ->
            updatetuple1
                |> Update.Extra.andThen update
                    (AddUndo changed
                        name
                        model
                    )


replaceselectedsymbols : List Symbol -> Symbol -> List Symbol
replaceselectedsymbols syms symbol =
    List.map (\sym -> replaceselected sym symbol) syms


replaceselected : Symbol -> Symbol -> Symbol
replaceselected sym symbol =
    if not sym.selected then
        sym
    else
        { sym
            | width = symbol.width
            , height = symbol.height
            , nwcolor = symbol.nwcolor
            , key = symbol.key
            , nbcolor = symbol.nbcolor
        }


symbolshavechanged : List a -> List a -> Bool
symbolshavechanged firstsymbols secondsymbols =
    not <| List.Extra.isPermutationOf firstsymbols secondsymbols


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.downs MouseDown
        , Mouse.moves MouseMove
        , Mouse.ups MouseUp
        , subaddsigntosignview SetSign
        , receiveElementPosition ReceiveElementPosition
        , subDragSymbol DragSymbol
        , subAddSymbol AddSymbol
        , subReplaceSymbol ReplaceSymbol
        , receiveKeyboardCommand Keyboard
        ]



-- To nest subscriptions
-- Sub.batch
--       [ SubSWEditor.State.subscriptions model.subSWEditorFieldName |> Sub.map SubSWEditorMsg
--       ]
