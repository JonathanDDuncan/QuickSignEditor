module SWEditor.EditorKeyboard exposing (..)

import SWEditor.Types exposing (..)
import Keyboard.Shared exposing (..)


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


configKeyboardSignView : List (KeyConfig Msg)
configKeyboardSignView =
    [ { test = { key = 43, special = [ .ctrlPressed ] }
      , action = (Undo)
      }
    , { test = { key = 21, special = [ .ctrlPressed ] }
      , action = (Redo)
      }
    , { test = { key = 62, special = [] }
      , action = (DeleteSymbols)
      }
    , { test = { key = 67, special = [] }
      , action = (MoveSymbols Up 1)
      }
    , { test = { key = 69, special = [] }
      , action = (MoveSymbols Down 1)
      }
    , { test = { key = 70, special = [] }
      , action = (MoveSymbols Right 1)
      }
    , { test = { key = 68, special = [] }
      , action = (MoveSymbols Left 1)
      }
    , { test = { key = 67, special = [ .ctrlPressed ] }
      , action = (MoveSymbols Up 10)
      }
    , { test = { key = 69, special = [ .ctrlPressed ] }
      , action = (MoveSymbols Down 10)
      }
    , { test = { key = 70, special = [ .ctrlPressed ] }
      , action = (MoveSymbols Right 10)
      }
    , { test = { key = 68, special = [ .ctrlPressed ] }
      , action = (MoveSymbols Left 10)
      }
    ]


runKeyboardGeneralChooser model command update =
    model ! []


runKeyboardGroupChooser model command update =
    model ! []


runKeyboardSymbolChooser model command update =
    model ! []
