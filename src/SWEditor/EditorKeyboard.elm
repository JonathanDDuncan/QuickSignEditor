module SWEditor.EditorKeyboard exposing (..)

import SWEditor.Types exposing (..)
import Keyboard.Shared exposing (..)
import Html exposing (text)


runKeyboardCommand :
    Model
    -> KeyboardCommand
    -> (Msg -> Model -> ( Model, Cmd Msg ))
    -> ( Model, Cmd Msg )
runKeyboardCommand model command update =
    let
        mode =
            getKeyboardMode command.mode

        updatetuple =
            case mode of
                SignView ->
                    runKeyboard model command update configKeyboardSignView

                GeneralChooser ->
                    runKeyboardGeneralChooser model command update

                GroupChooser ->
                    runKeyboardGroupChooser model command update

                SymbolChooser ->
                    runKeyboardSymbolChooser model command update
    in
        updatetuple



--To be used with keyboard layout
--Data still needs to be sent over somehow


keyboarddisplay : { signview : List { display : Html.Html Msg, test : KeyTestConfig } }
keyboarddisplay =
    { signview = List.map (\config -> { test = config.test, display = config.display }) configKeyboardSignView }


configKeyboardSignView : List (KeyConfig Msg)
configKeyboardSignView =
    [ { test = { key = 43, special = [ .ctrlPressed ] }
      , action = (Undo)
      , display = text "43"
      }
    , { test = { key = 21, special = [ .ctrlPressed ] }
      , action = (Redo)
      , display = text "43"
      }
    , { test = { key = 62, special = [] }
      , action = (DeleteSymbols)
      , display = text "43"
      }
    , { test = { key = 67, special = [] }
      , action = (MoveSymbols Up 1)
      , display = text "43"
      }
    , { test = { key = 69, special = [] }
      , action = (MoveSymbols Down 1)
      , display = text "43"
      }
    , { test = { key = 70, special = [] }
      , action = (MoveSymbols Right 1)
      , display = text "43"
      }
    , { test = { key = 68, special = [] }
      , action = (MoveSymbols Left 1)
      , display = text "43"
      }
    , { test = { key = 67, special = [ .ctrlPressed ] }
      , action = (MoveSymbols Up 10)
      , display = text "43"
      }
    , { test = { key = 69, special = [ .ctrlPressed ] }
      , action = (MoveSymbols Down 10)
      , display = text "43"
      }
    , { test = { key = 70, special = [ .ctrlPressed ] }
      , action = (MoveSymbols Right 10)
      , display = text "43"
      }
    , { test = { key = 68, special = [ .ctrlPressed ] }
      , action = (MoveSymbols Left 10)
      , display = text "43"
      }
    ]


runKeyboardGeneralChooser model command update =
    model ! []


runKeyboardGroupChooser model command update =
    model ! []


runKeyboardSymbolChooser model command update =
    model ! []
