module SWEditor.EditorKeyboard exposing (..)

import Update.Extra exposing (..)
import SWEditor.Types exposing (..)
import Keyboard.Shared exposing (..)


runKeyboardCommand model command update =
    let
        mode =
            getKeyboardMode command.mode

        updatetuple =
            case mode of
                SignView ->
                    runKeyboardSignView model command update

                GeneralChooser ->
                    runKeyboardGeneralChooser model command update

                GroupChooser ->
                    runKeyboardGroupChooser model command update

                SymbolChooser ->
                    runKeyboardSymbolChooser model command update
    in
        updatetuple


runKeyboardSignView model command update =
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


runKeyboardGeneralChooser model command update =
    model ! []


runKeyboardGroupChooser model command update =
    model ! []


runKeyboardSymbolChooser model command update =
    model ! []
