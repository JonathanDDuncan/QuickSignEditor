module Choosers.EditorUpdate exposing (editorupdate)

import Choosers.Types exposing (Model, Msg(KeyboardMsg, UpdateHandSymbolChooser), Update)
import Choosers.KeyboardType as KeyboardType exposing (KeyboardType)
import Choosers.EditorType exposing (Editor(..))
import Ports exposing (cmdAddSymbol, cmdDragSymbol, cmdReplaceSymbol)
import SW.Symbol exposing (createSymbolbyKey)
import Update.Extra
import Keyboard.KeyboardModeType as KeyboardMode exposing (KeyboardMode)


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
                |> Update.Extra.andThen update ((KeyboardMsg << KeyboardType.SetKeyboardMode) KeyboardMode.GroupChooser)

        GroupSelected choosing ->
            ( { model
                | groupselected = choosing
              }
            , Cmd.none
            )
                |> Update.Extra.andThen update (KeyboardMsg <| KeyboardType.UpdateChooserKeyboards)
                |> Update.Extra.andThen update UpdateHandSymbolChooser
                |> Update.Extra.andThen update ((KeyboardMsg << KeyboardType.SetKeyboardMode) KeyboardMode.SymbolChooser)

        AddSymbol key ->
            ( model
            , cmdAddSymbol <| createSymbolbyKey key model.symbolsizes
            )
                |> Update.Extra.andThen update ((KeyboardMsg << KeyboardType.SetKeyboardMode) KeyboardMode.SignView)

        DragSymbol key ->
            ( model
            , cmdDragSymbol <| createSymbolbyKey key model.symbolsizes
            )

        ReplaceSymbol key ->
            ( model
            , cmdReplaceSymbol <| createSymbolbyKey key model.symbolsizes
            )
