module SWEditor.EditorKeyboard exposing (runKeyboardCommand, configKeyboardSignView)

import SWEditor.Types exposing (Model, Msg(Undo, Redo, DeleteSymbols, DuplicateSymbols, MoveSymbols), Direction(..))
import Keyboard.Shared exposing (KeyboardCommand, KeyAction, getKeyboardMode, runKeyboard)
import Keyboard.Shared as KeyboardMode exposing (KeyboardMode)
import List.Extra
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
    let
        mode =
            getKeyboardMode command.mode

        updatetuple =
            if mode == KeyboardMode.SignView then
                runKeyboard model command update configKeyboardSignView
            else
                model ! []
    in
        updatetuple


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


getkeyaction dist zoom shift range =
    let
        values =
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

        keyrange =
            List.Extra.zip range values

        viewkeylist =
            List.map
                (\( key, value ) ->
                    { value
                        | test = { key = key, ctrl = False, shift = shift, alt = False }
                    }
                )
                keyrange
    in
        viewkeylist


arrowupdisplay scale =
    { width = 24
    , height = 24
    , backgroundcolor = Nothing
    , view =
        arrowupicon scale
    }


arrowdowndisplay scale =
    { width = 24
    , height = 24
    , backgroundcolor = Nothing
    , view =
        arrowdownicon scale
    }


arrowrightdisplay scale =
    { width = 18
    , height = 18
    , backgroundcolor = Nothing
    , view =
        arrowrighticon scale
    }


arrowleftdisplay scale =
    { width = 18
    , height = 18
    , backgroundcolor = Nothing
    , view =
        arrowlefticon scale
    }
