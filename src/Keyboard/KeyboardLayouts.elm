module Keyboard.KeyboardLayouts exposing (..)

import Keyboard.Types exposing (..)
import Dict exposing (..)
import SW.State exposing (..)
import SWEditor.EditorSign exposing (..)


fingerspellingQueryAsl :
    { keys : List { code : number, keypress : Keypress, sign : EditorSign }
    , name : String
    }
fingerspellingQueryAsl =
    { name = "quertyASL", keys = keyboardDisplay querty.keys aslTemplate }


keyboardDisplay :
    List a
    -> b
    -> List { code : number, keypress : Keypress, sign : EditorSign }
keyboardDisplay keyboardlayout keyboardtemplate =
    let
        display =
            List.map (createKeyDisplay keyboardtemplate) keyboardlayout
    in
        display


createKeyDisplay :
    b
    -> c
    -> { code : number, keypress : Keypress, sign : EditorSign }
createKeyDisplay keyboardtemplate key =
    { sign = SW.State.signinit
    , code = 0
    , keypress = None
    }


aslTemplate : List { fsw : String, letter : String, sign : Maybe EditorSign }
aslTemplate =
    [ { letter = "A", fsw = "M507x507S1f720487x492", sign = Nothing }
    , { letter = "B", fsw = "M507x507S14720493x485", sign = Nothing }
    , { letter = "C", fsw = "M508x507S16d20491x487", sign = Nothing }
    , { letter = "D", fsw = "M508x507S10120492x477", sign = Nothing }
    , { letter = "E", fsw = "M507x507S14a20492x492", sign = Nothing }
    , { letter = "F", fsw = "M513x507S1ce20491x477", sign = Nothing }
    , { letter = "G", fsw = "M507x507S1f000478x492", sign = Nothing }
    , { letter = "H", fsw = "M507x507S11502477x492", sign = Nothing }
    , { letter = "I", fsw = "M513x507S19220492x488", sign = Nothing }
    , { letter = "J", fsw = "M513x507S19220492x488S2a20c476x472", sign = Nothing }
    , { letter = "K", fsw = "M507x507S14020478x477", sign = Nothing }
    , { letter = "L", fsw = "M508x507S1dc20484x477", sign = Nothing }
    , { letter = "M", fsw = "M507x507S18d20487x482", sign = Nothing }
    , { letter = "N", fsw = "M507x507S11920486x481", sign = Nothing }
    , { letter = "O", fsw = "M508x507S17620492x491", sign = Nothing }
    , { letter = "P", fsw = "M510x509S14051479x485", sign = Nothing }
    , { letter = "Q", fsw = "M511x509S1f051481x486", sign = Nothing }
    , { letter = "R", fsw = "M507x507S11a20492x477", sign = Nothing }
    , { letter = "S", fsw = "M507x507S20320492x492", sign = Nothing }
    , { letter = "T", fsw = "M508x507S1fb20493x488", sign = Nothing }
    , { letter = "U", fsw = "M507x507S11520492x477", sign = Nothing }
    , { letter = "V", fsw = "M508x507S10e20493x477", sign = Nothing }
    , { letter = "W", fsw = "M509x507S18620491x478", sign = Nothing }
    , { letter = "X", fsw = "M508x507S10620487x481", sign = Nothing }
    , { letter = "Y", fsw = "M514x507S19a20486x487", sign = Nothing }
    , { letter = "Z", fsw = "M528x507S10020492x477S2450a497x473", sign = Nothing }
    ]


querty : KeyboardLayout
querty =
    { name = "QWERTY"
    , keys =
        [ { code = 0, display = " ", keypress = None }
        , { code = 192, display = "`", keypress = None }
        , { code = 49, display = "1", keypress = None }
        , { code = 50, display = "2", keypress = None }
        , { code = 51, display = "3", keypress = None }
        , { code = 52, display = "4", keypress = None }
        , { code = 53, display = "5", keypress = None }
        , { code = 54, display = "6", keypress = None }
        , { code = 55, display = "7", keypress = None }
        , { code = 56, display = "8", keypress = None }
        , { code = 57, display = "9", keypress = None }
        , { code = 48, display = "0", keypress = None }
        , { code = 189, display = "-", keypress = None }
        , { code = 187, display = "=", keypress = None }
        , { code = 0, display = "Backspace", keypress = None }
        , { code = 0, display = "Tab", keypress = None }
        , { code = 81, display = "Q", keypress = None }
        , { code = 87, display = "W", keypress = None }
        , { code = 69, display = "E", keypress = None }
        , { code = 82, display = "R", keypress = None }
        , { code = 84, display = "T", keypress = None }
        , { code = 89, display = "Y", keypress = None }
        , { code = 85, display = "U", keypress = None }
        , { code = 73, display = "I", keypress = None }
        , { code = 79, display = "O", keypress = None }
        , { code = 80, display = "P", keypress = None }
        , { code = 219, display = "[", keypress = None }
        , { code = 221, display = "]", keypress = None }
        , { code = 220, display = "\\", keypress = None }
        , { code = 0, display = "Caps", keypress = None }
        , { code = 65, display = "A", keypress = None }
        , { code = 83, display = "S", keypress = None }
        , { code = 68, display = "D", keypress = None }
        , { code = 70, display = "F", keypress = None }
        , { code = 71, display = "G", keypress = None }
        , { code = 72, display = "H", keypress = None }
        , { code = 74, display = "J", keypress = None }
        , { code = 75, display = "K", keypress = None }
        , { code = 76, display = "L", keypress = None }
        , { code = 186, display = ";", keypress = None }
        , { code = 222, display = "'", keypress = None }
        , { code = 13, display = "Enter", keypress = None }
        , { code = 0, display = "Shift", keypress = None }
        , { code = 90, display = "Z", keypress = None }
        , { code = 88, display = "X", keypress = None }
        , { code = 67, display = "C", keypress = None }
        , { code = 86, display = "V", keypress = None }
        , { code = 66, display = "B", keypress = None }
        , { code = 78, display = "N", keypress = None }
        , { code = 77, display = "M", keypress = None }
        , { code = 188, display = ",", keypress = None }
        , { code = 190, display = ".", keypress = None }
        , { code = 191, display = "/", keypress = None }
        , { code = 0, display = "Shift", keypress = None }
        , { code = 0, display = "Ctrl", keypress = None }
        , { code = 0, display = "Cmd", keypress = None }
        , { code = 0, display = "Alt", keypress = None }
        , { code = 32, display = "Space", keypress = None }
        , { code = 0, display = "Alt ", keypress = None }
        , { code = 0, display = "Menu Key", keypress = None }
        , { code = 0, display = "Ctrl", keypress = None }
        , { code = 45, display = "Insert", keypress = None }
        , { code = 36, display = "Delete", keypress = None }
        , { code = 33, display = "Home", keypress = None }
        , { code = 46, display = "End", keypress = None }
        , { code = 35, display = "PgUp", keypress = None }
        , { code = 3, display = "PgDown", keypress = None }
        , { code = 38, display = "↑", keypress = None }
        , { code = 37, display = "←", keypress = None }
        , { code = 40, display = "↓", keypress = None }
        , { code = 39, display = "→", keypress = None }
        , { code = 47, display = "/", keypress = None }
        , { code = 42, display = "*", keypress = None }
        , { code = 45, display = "-", keypress = None }
        , { code = 55, display = "7", keypress = None }
        , { code = 56, display = "8", keypress = None }
        , { code = 57, display = "9", keypress = None }
        , { code = 43, display = "+", keypress = None }
        , { code = 52, display = "4", keypress = None }
        , { code = 53, display = "5", keypress = None }
        , { code = 54, display = "6", keypress = None }
        , { code = 49, display = "1", keypress = None }
        , { code = 50, display = "2", keypress = None }
        , { code = 51, display = "3", keypress = None }
        , { code = 48, display = "0", keypress = None }
        , { code = 46, display = ".", keypress = None }
        , { code = 13, display = "Enter", keypress = None }
        ]
    }


keycodes : Dict Int String
keycodes =
    Dict.fromList
        [ ( 8, "backspace" )
        , ( 9, "tab" )
        , ( 13, "enter" )
        , ( 16, "shift" )
        , ( 17, "ctrl" )
        , ( 18, "alt" )
        , ( 19, "pause/break" )
        , ( 20, "caps lock" )
        , ( 27, "escape" )
        , ( 32, "Space" )
        , ( 33, "page up" )
        , ( 34, "page down" )
        , ( 35, "end" )
        , ( 36, "home" )
        , ( 37, "←" )
        , ( 38, "↑" )
        , ( 39, "→" )
        , ( 40, "↓" )
        , ( 42, "*" )
        , ( 43, "+" )
        , ( 45, "-" )
        , ( 46, "." )
        , ( 47, "/" )
        , ( 48, "0" )
        , ( 49, "1" )
        , ( 50, "2" )
        , ( 51, "3" )
        , ( 52, "4" )
        , ( 53, "5" )
        , ( 54, "6" )
        , ( 55, "7" )
        , ( 56, "8" )
        , ( 57, "9" )
        , ( 65, "A" )
        , ( 66, "B" )
        , ( 67, "C" )
        , ( 68, "D" )
        , ( 69, "E" )
        , ( 70, "F" )
        , ( 71, "G" )
        , ( 72, "H" )
        , ( 73, "I" )
        , ( 74, "J" )
        , ( 75, "K" )
        , ( 76, "L" )
        , ( 77, "M" )
        , ( 78, "N" )
        , ( 79, "O" )
        , ( 80, "P" )
        , ( 81, "Q" )
        , ( 82, "R" )
        , ( 83, "S" )
        , ( 84, "T" )
        , ( 85, "U" )
        , ( 86, "V" )
        , ( 87, "W" )
        , ( 88, "X" )
        , ( 89, "Y" )
        , ( 90, "Z" )
        , ( 91, "left window key" )
        , ( 92, "right window key" )
        , ( 93, "select key" )
        , ( 96, "numpad 0" )
        , ( 97, "numpad 1" )
        , ( 98, "numpad 2" )
        , ( 99, "numpad 3" )
        , ( 100, "numpad 4" )
        , ( 101, "numpad 5" )
        , ( 102, "numpad 6" )
        , ( 103, "numpad 7" )
        , ( 104, "numpad 8" )
        , ( 105, "numpad 9" )
        , ( 106, "multiply" )
        , ( 107, "add" )
        , ( 109, "subtract" )
        , ( 110, "decimal point" )
        , ( 111, "divide" )
        , ( 112, "f1" )
        , ( 113, "f2" )
        , ( 114, "f3" )
        , ( 115, "f4" )
        , ( 116, "f5" )
        , ( 117, "f6" )
        , ( 118, "f7" )
        , ( 119, "f8" )
        , ( 120, "f9" )
        , ( 121, "f10" )
        , ( 122, "f11" )
        , ( 123, "f12" )
        , ( 144, "num lock" )
        , ( 145, "scroll lock" )
        , ( 186, ";" )
        , ( 187, "=" )
        , ( 188, "," )
        , ( 189, "-" )
        , ( 190, "." )
        , ( 191, "/" )
        , ( 192, "`" )
        , ( 219, "[" )
        , ( 220, "\\" )
        , ( 221, "]" )
        , ( 222, "'" )
        ]