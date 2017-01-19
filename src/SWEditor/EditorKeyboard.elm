module SWEditor.EditorKeyboard exposing (..)

import SWEditor.Types exposing (..)
import Keyboard.Shared exposing (..)
import Html exposing (text, i)
import Svg exposing (svg, path)
import Html.Attributes exposing (..)
import ViewHelper.ViewExtra exposing (..)


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
      , display =
            svg [ attribute "height" "24", attribute "viewBox" "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
                [ path [ attribute "d" "M12.5 8c-2.65 0-5.05.99-6.9 2.6L2 7v9h9l-3.62-3.62c1.39-1.16 3.16-1.88 5.12-1.88 3.54 0 6.55 2.31 7.6 5.5l2.37-.78C21.08 11.03 17.15 8 12.5 8z" ]
                    []
                ]
      }
    , { test = { key = 21, special = [ .ctrlPressed ] }
      , action = (Redo)
      , display =
            svg [ attribute "height" "24", attribute "viewBox" "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
                [ path [ attribute "d" "M18.4 10.6C16.55 8.99 14.15 8 11.5 8c-4.65 0-8.58 3.03-9.96 7.22L3.9 16c1.05-3.19 4.05-5.5 7.6-5.5 1.95 0 3.73.72 5.12 1.88L13 16h9V7l-3.6 3.6z" ]
                    []
                ]
      }
    , { test = { key = 62, special = [] }
      , action = (DeleteSymbols)
      , display = text "43"
      }
    , { test = { key = 67, special = [] }
      , action = (MoveSymbols Up 1)
      , display =
            svg [ attribute "height" "24", attribute "viewBox" "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
                [ path [ attribute "d" "M4 12l1.41 1.41L11 7.83V20h2V7.83l5.58 5.59L20 12l-8-8-8 8z" ]
                    []
                ]
      }
    , { test = { key = 69, special = [] }
      , action = (MoveSymbols Down 1)
      , display =
            svg [ attribute "height" "24", attribute "viewBox" "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
                [ path [ attribute "d" "M20 12l-1.41-1.41L13 16.17V4h-2v12.17l-5.58-5.59L4 12l8 8 8-8z", attribute "fill" "#010101" ]
                    []
                ]
      }
    , { test = { key = 70, special = [] }
      , action = (MoveSymbols Right 1)
      , display =
            svg [ attribute "height" "18", attribute "viewBox" "0 0 18 18", attribute "width" "18", attribute "xmlns" "http://www.w3.org/2000/svg" ]
                [ path [ attribute "d" "M9 3L7.94 4.06l4.19 4.19H3v1.5h9.13l-4.19 4.19L9 15l6-6z" ]
                    []
                ]
      }
    , { test = { key = 68, special = [] }
      , action = (MoveSymbols Left 1)
      , display =
            svg [ attribute "height" "18", attribute "viewBox" "0 0 18 18", attribute "width" "18", attribute "xmlns" "http://www.w3.org/2000/svg" ]
                [ path [ attribute "d" "M15 8.25H5.87l4.19-4.19L9 3 3 9l6 6 1.06-1.06-4.19-4.19H15v-1.5z" ]
                    []
                ]
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
