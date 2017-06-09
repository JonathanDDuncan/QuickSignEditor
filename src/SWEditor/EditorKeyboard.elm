module SWEditor.EditorKeyboard exposing (runKeyboardCommand, configKeyboardSignView)

import SWEditor.Types exposing (Model, Msg(Undo, Redo, DeleteSymbols, DuplicateSymbols, MoveSymbols), Direction(..))
import Keyboard.Shared exposing (KeyboardCommand, KeyAction, getKeyboardMode, runKeyboard)
import Keyboard.Shared as KeyboardMode exposing (KeyboardMode)
import List.Extra
import Html
import SWEditor.Icons
    exposing
        ( undoicon
        , redoicon
        , garbagecanicon
        , duplicateicon
        , arrowupicon
        , arrowdownicon
        , arrowrighticon
        , arrowlefticon
        )


runKeyboardCommand :
    Model
    -> KeyboardCommand
    -> (Msg -> Model -> ( Model, Cmd Msg ))
    -> ( Model, Cmd Msg )
runKeyboardCommand model command update =
    if (getKeyboardMode command.mode) == KeyboardMode.SignView then
        runKeyboard model command update configKeyboardSignView
    else
        model ! []


configKeyboardSignView : List (KeyAction Msg)
configKeyboardSignView =
    List.append otherkeys arrowkeys


otherkeys : List (KeyAction Msg)
otherkeys =
    [ { test = { key = 43, ctrl = True, shift = False, alt = False }
      , action = Undo
      , display =
            { width = 24
            , height = 24
            , backgroundcolor = Nothing
            , view =
                undoicon
            }
      }
    , { test = { key = 21, ctrl = True, shift = False, alt = False }
      , action = Redo
      , display =
            { width = 24
            , height = 24
            , backgroundcolor = Nothing
            , view =
                redoicon
            }
      }
    , { test =
            -- Delete key
            { key = 62, ctrl = False, shift = False, alt = False }
      , action = DeleteSymbols
      , display =
            { width = 24
            , height = 24
            , backgroundcolor = Nothing
            , view =
                garbagecanicon
            }
      }
    , { test =
            -- Backspace
            { key = 14, ctrl = False, shift = False, alt = False }
      , action = DeleteSymbols
      , display =
            { width = 24
            , height = 24
            , backgroundcolor = Nothing
            , view =
                garbagecanicon
            }
      }
    , { test = { key = 33, ctrl = False, shift = False, alt = False }
      , action = DuplicateSymbols
      , display =
            { width = 24
            , height = 24
            , backgroundcolor = Nothing
            , view =
                duplicateicon
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


getkeyaction :
    SWEditor.Types.Distance
    -> b
    -> c
    -> List a
    -> List
        { action : Msg
        , display :
            { backgroundcolor : Maybe a1
            , height : number
            , view : Html.Html msg
            , width : number1
            }
        , test : { alt : Bool, ctrl : Bool, key : a, shift : c }
        }
getkeyaction dist zoom shift range =
    [ { test = { key = 0, ctrl = False, shift = False, alt = False }
      , action = MoveSymbols Up dist
      , display = arrowupdisplay zoom
      }
    , { test = { key = 0, ctrl = False, shift = False, alt = False }
      , action = MoveSymbols Down dist
      , display = arrowdowndisplay zoom
      }
    , { test = { key = 0, ctrl = False, shift = False, alt = False }
      , action = MoveSymbols Right dist
      , display = arrowrightdisplay zoom
      }
    , { test = { key = 0, ctrl = False, shift = False, alt = False }
      , action = MoveSymbols Left dist
      , display = arrowleftdisplay zoom
      }
    ]
        |> List.Extra.zip range
        |> List.map
            (\( key, value ) ->
                { value
                    | test = { key = key, ctrl = False, shift = shift, alt = False }
                }
            )


arrowupdisplay :
    b
    -> { backgroundcolor : Maybe a
       , height : number
       , view : Html.Html msg
       , width : number1
       }
arrowupdisplay scale =
    { width = 24
    , height = 24
    , backgroundcolor = Nothing
    , view = arrowupicon scale
    }


arrowdowndisplay :
    b
    -> { backgroundcolor : Maybe a
       , height : number
       , view : Html.Html msg
       , width : number1
       }
arrowdowndisplay scale =
    { width = 24
    , height = 24
    , backgroundcolor = Nothing
    , view = arrowdownicon scale
    }


arrowrightdisplay :
    b
    -> { backgroundcolor : Maybe a
       , height : number
       , view : Html.Html msg
       , width : number1
       }
arrowrightdisplay scale =
    { width = 18
    , height = 18
    , backgroundcolor = Nothing
    , view = arrowrighticon scale
    }


arrowleftdisplay :
    b
    -> { backgroundcolor : Maybe a
       , height : number
       , view : Html.Html msg
       , width : number1
       }
arrowleftdisplay scale =
    { width = 18
    , height = 18
    , backgroundcolor = Nothing
    , view = arrowlefticon scale
    }
