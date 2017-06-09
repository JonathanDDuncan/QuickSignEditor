module Choosers.Editor exposing (editorupdate)

import Choosers.Types exposing (Model, Msg(KeyboardMsg, UpdateHandSymbolChooser), ChoosingImportModel, Update, Editor(..))
import Choosers.Types as KeyboardType exposing (KeyboardType)
import Choosers.Types exposing (Loading)
import SW.Symbol exposing (Hands)
import SW.Symbol exposing (HandFills)
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
            ( model
            , cmdAddSymbol <| getSymbolbyKey key model.symbolsizes
            )
                |> Update.Extra.andThen update ((KeyboardMsg << KeyboardType.SetKeyboardMode) Keyboard.Shared.SignView)

        DragSymbol key ->
            ( model
            , cmdDragSymbol <| getSymbolbyKey key model.symbolsizes
            )

        ReplaceSymbol key ->
            ( model
            , cmdReplaceSymbol <| getSymbolbyKey key model.symbolsizes
            )
