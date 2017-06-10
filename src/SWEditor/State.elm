module SWEditor.State exposing (init, update, subscriptions)

import Ports exposing (requestSignfromOtherApp, requestElementPosition, sendKeyboardMode, subaddsigntosignview, receiveElementPosition, subDragSymbol, subAddSymbol, subReplaceSymbol, receiveKeyboardCommand)
import Update.Extra
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
import SWEditor.EditorSymbol exposing (symbolId)
import SWEditor.Undo exposing (addUndo, undo, redo)
import SW.Types exposing (Position)
import SW.Sign exposing (signinit, lastsignid, refreshsign, centersignarea)
import SW.PortableSign exposing (portableSigntoSign)
import SW.Symbol exposing (Symbol, symbolsUnderPosition, countselectedsymbols)
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
    ( { fsw = ""
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

        -- RequestSign ->
        --     ( model, requestSign model.fsw )
        -- RequestSignfromOtherApp ->
        --     ( model, requestSignfromOtherApp "" )
        SetSign portablesign ->
            let
                editorSign =
                    refreshsign model.uid (portableSigntoSign portablesign)

                lastuid =
                    editorSign
                        |> lastsignid
            in
                { model | sign = editorSign, uid = lastuid }
                    ! []
                    |> Update.Extra.andThen update UpdateSignViewPosition

        RequestElementPosition elementid ->
            ( model, requestElementPosition elementid )

        ReceiveElementPosition namedposition ->
            let
                adjustedformargin =
                    { namedposition
                        | x = namedposition.x + model.signviewmargin
                        , y = namedposition.y + model.signviewmargin
                    }
            in
                { model | viewposition = adjustedformargin }
                    ! []
                    |> Update.Extra.andThen update CenterSign

        UpdateSignViewPosition ->
            { model | windowresized = False }
                ! []
                |> Update.Extra.andThen update (RequestElementPosition "signView")

        CenterSign ->
            { model | sign = centersignarea model.viewposition model.sign }
                ! []
                |> addUndoEntry True "CenterSign"

        StartRectangleSelect ->
            { model
                | editormode = RectangleSelect
                , rectanglestart = model.xy
            }
                ! []

        EndRectangleSelect ->
            let
                newsign =
                    rectangleselect model

                changed =
                    symbolshavechanged model.sign.syms newsign.syms
            in
                { model | editormode = Awaiting, sign = newsign }
                    ! []
                    |> addUndoEntry changed "EndRectangleSelect"

        SelectSymbol id ->
            let
                newsign =
                    selectSymbolId id model.sign

                changed =
                    symbolshavechanged model.sign.syms newsign.syms
            in
                { model | sign = newsign }
                    ! []
                    |> addUndoEntry changed "SelectSymbol"
                    |> Update.Extra.andThen update StartDragging

        UnSelectSymbols ->
            let
                newsign =
                    unselectSignSymbols model.sign

                changed =
                    symbolshavechanged model.sign.syms newsign.syms
            in
                { model | sign = newsign }
                    ! []
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

        MouseUp _ ->
            model
                ! []
                |> Update.Extra.filter (model.editormode == RectangleSelect)
                    (Update.Extra.andThen update EndRectangleSelect)
                |> Update.Extra.filter (model.editormode == Dragging || model.editormode == AddingSymbol)
                    (Update.Extra.andThen update EndDragging)

        MouseMove position ->
            { model | xy = signViewPosition position model.viewposition model.signviewmargin }
                ! []
                |> Update.Extra.filter model.windowresized
                    (Update.Extra.andThen update UpdateSignViewPosition)
                |> Update.Extra.filter (model.editormode == Dragging || model.editormode == AddingSymbol)
                    (Update.Extra.andThen update DragSelected)

        ReplaceSymbol symbol ->
            let
                sign1 =
                    model.sign
            in
                { model
                    | sign =
                        { sign1
                            | syms =
                                if countselectedsymbols model.sign.syms == 1 then
                                    replaceselectedsymbols model.sign.syms symbol
                                else
                                    model.sign.syms
                        }
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
            in
                { model
                    | uid = lastuid
                    , editormode = Awaiting
                    , sign = sign
                }
                    ! []
                    |> addUndoEntry True "AddSymbol"

        EndDragging ->
            let
                undroppediswithinview =
                    getundroppedsymbol model.editormode model.sign.syms
                        |> issymbolwithinview model.viewposition

                newmodel =
                    if not undroppediswithinview then
                        deletesymbols model
                    else
                        model

                signwithinbounds =
                    putsymbolswithinbounds newmodel.sign newmodel.viewposition
            in
                { newmodel
                    | sign = signwithinbounds
                    , editormode = Awaiting
                }
                    ! []
                    |> addUndoEntry True "EndDragging"

        AddUndo changed actiononame model1 ->
            addUndo changed actiononame model1 ! []

        Undo ->
            undo model ! []

        Redo ->
            redo model ! []

        DeleteSymbols ->
            deletesymbols model
                ! []
                |> addUndoEntry True "DeleteSymbols"

        DuplicateSymbols ->
            duplicatesymbols model
                ! []
                |> addUndoEntry True "DuplicateSymbols"

        SizeIncreaseSymbols ->
            changesizesymbols model 0.1
                ! []
                |> addUndoEntry True "SizeIncreaseSymbols"

        SizeDecreaseSymbols ->
            changesizesymbols model -0.1
                ! []
                |> addUndoEntry True "SizeDecreaseSymbols"

        Keyboard command ->
            runKeyboardCommand model command update

        MoveSymbols direction distance ->
            movesymbols model direction distance
                ! []
                |> addUndoEntry True "MoveSymbols"

        SetKeyboardMode mode ->
            ( model
            , sendKeyboardMode <| Keyboard.Shared.getKeyboardModeCode mode
            )


addUndoEntry :
    Bool
    -> String
    -> ( Model, Cmd Msg )
    -> ( Model, Cmd Msg )
addUndoEntry changed name (( model, _ ) as updatetuple) =
    updatetuple
        |> Update.Extra.andThen update
            (AddUndo changed name model)


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
    List.Extra.isPermutationOf firstsymbols secondsymbols
        |> not


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
