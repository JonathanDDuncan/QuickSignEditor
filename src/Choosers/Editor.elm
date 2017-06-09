module Choosers.Editor exposing (editorupdate)

import Choosers.Types exposing (Model, Msg(KeyboardMsg, UpdateHandSymbolChooser), ChoosingImportModel, Update, Editor(..))
import Choosers.Types as KeyboardType exposing (KeyboardType)
import Choosers.Types exposing (Loading)
import Choosers.Types exposing (Hands)
import Choosers.Types exposing (HandFills)
import Ports exposing (cmdAddSymbol, cmdDragSymbol, cmdReplaceSymbol)
import SWEditor.EditorSymbol exposing (getSymbolbyKey)
import Update.Extra
import Keyboard.Shared exposing (KeyboardMode)


editorupdate : Editor -> Model -> Update -> ( Model, Cmd Msg )
editorupdate action model update =
    case action of
        SelectedColumn column ->
            ( { model
                | selectedcolumn = column
              }
            , Cmd.none
            )
                |> Update.Extra.andThen update (KeyboardMsg <| KeyboardType.UpdateChooserKeyboards)

        Clicked clickvalue ->
            ( { model | clicked = clickvalue }
            , Cmd.none
            )
                |> Update.Extra.andThen update (KeyboardMsg <| KeyboardType.UpdateChooserKeyboards)
                |> Update.Extra.andThen update ((KeyboardMsg << KeyboardType.SetKeyboardMode) Keyboard.Shared.GroupChooser)

        GroupSelected choosing ->
            ( { model
                | groupselected = choosing
              }
            , Cmd.none
            )
                |> Update.Extra.andThen update (KeyboardMsg <| KeyboardType.UpdateChooserKeyboards)
                |> Update.Extra.andThen update UpdateHandSymbolChooser
                |> Update.Extra.andThen update ((KeyboardMsg << KeyboardType.SetKeyboardMode) Keyboard.Shared.SymbolChooser)

        AddSymbol key ->
            let
                editorsymbol =
                    getSymbolbyKey key model.symbolsizes
            in
                ( model
                , cmdAddSymbol editorsymbol
                )
                    |> Update.Extra.andThen update ((KeyboardMsg << KeyboardType.SetKeyboardMode) Keyboard.Shared.SignView)

        DragSymbol key ->
            let
                editorsymbol =
                    getSymbolbyKey key model.symbolsizes
            in
                ( model
                , cmdDragSymbol editorsymbol
                )

        ReplaceSymbol key ->
            let
                editorsymbol =
                    getSymbolbyKey key model.symbolsizes
            in
                ( model
                , cmdReplaceSymbol editorsymbol
                )
