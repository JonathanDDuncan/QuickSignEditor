module SWEditor.EditorKeyboard exposing (..)

import SWEditor.Types exposing (..)
import Keyboard.Shared exposing (..)
import Svg exposing (svg, path)
import Html.Attributes exposing (..)
import List.Extra


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
            if mode == SignView then
                runKeyboard model command update configKeyboardSignView
            else
                model ! []
    in
        updatetuple


keyboarddisplay :
    { signview :
        List (KeyConfig Msg)
    }
keyboarddisplay =
    { signview = List.map (\config -> { test = config.test, display = { width = 20, height = 20, view = config.display.view } }) configKeyboardSignView }


configKeyboardSignView : List (KeyAction Msg)
configKeyboardSignView =
    List.append otherkeys arrowkeys


otherkeys : List (KeyAction Msg)
otherkeys =
    [ { test = { key = 43, ctrl = True, shift = False, alt = False }
      , action = (Undo)
      , display =
            { width = 24
            , height = 24
            , view =
                svg [ attribute "height" "24", attribute "viewBox" "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
                    [ path [ attribute "d" "M12.5 8c-2.65 0-5.05.99-6.9 2.6L2 7v9h9l-3.62-3.62c1.39-1.16 3.16-1.88 5.12-1.88 3.54 0 6.55 2.31 7.6 5.5l2.37-.78C21.08 11.03 17.15 8 12.5 8z" ]
                        []
                    ]
            }
      }
    , { test = { key = 21, ctrl = True, shift = False, alt = False }
      , action = (Redo)
      , display =
            { width = 24
            , height = 24
            , view =
                svg [ attribute "height" "24", attribute "viewBox" "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
                    [ path [ attribute "d" "M18.4 10.6C16.55 8.99 14.15 8 11.5 8c-4.65 0-8.58 3.03-9.96 7.22L3.9 16c1.05-3.19 4.05-5.5 7.6-5.5 1.95 0 3.73.72 5.12 1.88L13 16h9V7l-3.6 3.6z" ]
                        []
                    ]
            }
      }
    , { test = { key = 62, ctrl = False, shift = False, alt = False }
      , action = (DeleteSymbols)
      , display =
            { width = 24
            , height = 24
            , view =
                svg [ attribute "height" "24", attribute "viewBox" "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
                    [ path [ attribute "d" "M6 19c0 1.1.9 2 2 2h8c1.1 0 2-.9 2-2V7H6v12zM19 4h-3.5l-1-1h-5l-1 1H5v2h14V4z" ]
                        []
                    ]
            }
      }
    ]


arrowkeys : List (KeyAction Msg)
arrowkeys =
    let
        keyactionnormal =
            getkeyaction 1 1 False [ 67, 69, 70, 68 ]

        keyactionfast =
            getkeyaction 10 1.3 True [ 67, 69, 70, 68 ]

        keyactionnormalwasd =
            getkeyaction 1 1 False [ 17, 31, 32, 30 ]

        keyactionfastwasd =
            getkeyaction 10 1.3 True [ 17, 31, 32, 30 ]
    in
        List.concat [ keyactionnormal, keyactionfast, keyactionnormalwasd, keyactionfastwasd ]


getkeyaction dist zoom shift range =
    let
        values =
            [ { test = { key = 0, ctrl = False, shift = False, alt = False }
              , action = (MoveSymbols Up dist)
              , display = arrowupdisplay zoom
              }
            , { test = { key = 0, ctrl = False, shift = False, alt = False }
              , action = (MoveSymbols Down dist)
              , display = arrowdowndisplay zoom
              }
            , { test = { key = 0, ctrl = False, shift = False, alt = False }
              , action = (MoveSymbols Right dist)
              , display = arrowrightdisplay zoom
              }
            , { test = { key = 0, ctrl = False, shift = False, alt = False }
              , action = (MoveSymbols Left dist)
              , display = arrowleftdisplay zoom
              }
            ]

        keyrange =
            List.Extra.zip range values

        viewkeylist =
            List.map
                (\( key, value ) ->
                    { value
                        | test = { key = key, ctrl = False, shift = shift, alt = False }
                    }
                )
                (keyrange)
    in
        viewkeylist



--    { test = { key = 67, ctrl = ctrl, shift = False, alt = False }
--   ,


arrowupdisplay scale =
    { width = 24
    , height = 24
    , view =
        svg [ attribute "height" "24", attribute "viewBox" "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
            [ path [ attribute "transform" ("scale(" ++ toString scale ++ ")"), attribute "d" "M4 12l1.41 1.41L11 7.83V20h2V7.83l5.58 5.59L20 12l-8-8-8 8z" ]
                []
            ]
    }


arrowdowndisplay scale =
    { width = 24
    , height = 24
    , view =
        svg [ attribute "height" "24", attribute "viewBox" "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
            [ path [ attribute "transform" ("scale(" ++ toString scale ++ ")"), attribute "d" "M20 12l-1.41-1.41L13 16.17V4h-2v12.17l-5.58-5.59L4 12l8 8 8-8z", attribute "fill" "#010101" ]
                []
            ]
    }


arrowrightdisplay scale =
    { width = 18
    , height = 18
    , view =
        svg [ attribute "height" "18", attribute "viewBox" "0 0 18 18", attribute "width" "18", attribute "xmlns" "http://www.w3.org/2000/svg" ]
            [ path [ attribute "transform" ("scale(" ++ toString scale ++ ")"), attribute "d" "M9 3L7.94 4.06l4.19 4.19H3v1.5h9.13l-4.19 4.19L9 15l6-6z" ]
                []
            ]
    }


arrowleftdisplay scale =
    { width = 18
    , height = 18
    , view =
        svg [ attribute "height" "18", attribute "viewBox" "0 0 18 18", attribute "width" "18", attribute "xmlns" "http://www.w3.org/2000/svg" ]
            [ path [ attribute "transform" ("scale(" ++ toString scale ++ ")"), attribute "d" "M15 8.25H5.87l4.19-4.19L9 3 3 9l6 6 1.06-1.06-4.19-4.19H15v-1.5z" ]
                []
            ]
    }
