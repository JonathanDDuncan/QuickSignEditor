module Keyboard.KeyboardLayouts exposing (querty, fingerspellingQueryAsl)

import Keyboard.Types exposing (Model, Msg, Keypress(None), KeyboardLayout)
import SW.Types exposing (Sign, signinit)


fingerspellingQueryAsl :
    { keys : List { code : number, keypress : Keypress, sign : Sign }
    , name : String
    }
fingerspellingQueryAsl =
    { name = "quertyASL", keys = keyboardDisplay querty.keys aslTemplate }


keyboardDisplay :
    List a
    -> b
    -> List { code : number, keypress : Keypress, sign : Sign }
keyboardDisplay keyboardlayout keyboardtemplate =
    let
        display =
            List.map (\layout -> createKeyDisplay) keyboardlayout
    in
        display


createKeyDisplay : { code : number, keypress : Keypress, sign : Sign }
createKeyDisplay =
    { sign = signinit
    , code = 0
    , keypress = None
    }


aslTemplate : List { fsw : String, letter : String, sign : Maybe Sign }
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
        [ { keyId = 0, code = 0, display = " ", keypress = None }
        , { keyId = 1, code = 192, display = "`", keypress = None }
        , { keyId = 2, code = 49, display = "1", keypress = None }
        , { keyId = 3, code = 50, display = "2", keypress = None }
        , { keyId = 4, code = 51, display = "3", keypress = None }
        , { keyId = 5, code = 52, display = "4", keypress = None }
        , { keyId = 6, code = 53, display = "5", keypress = None }
        , { keyId = 7, code = 54, display = "6", keypress = None }
        , { keyId = 8, code = 55, display = "7", keypress = None }
        , { keyId = 9, code = 56, display = "8", keypress = None }
        , { keyId = 10, code = 57, display = "9", keypress = None }
        , { keyId = 11, code = 48, display = "0", keypress = None }
        , { keyId = 12, code = 189, display = "-", keypress = None }
        , { keyId = 13, code = 187, display = "=", keypress = None }
        , { keyId = 14, code = 8, display = "Backspace", keypress = None }
        , { keyId = 15, code = 9, display = "Tab", keypress = None }
        , { keyId = 16, code = 81, display = "Q", keypress = None }
        , { keyId = 17, code = 87, display = "W", keypress = None }
        , { keyId = 18, code = 69, display = "E", keypress = None }
        , { keyId = 19, code = 82, display = "R", keypress = None }
        , { keyId = 20, code = 84, display = "T", keypress = None }
        , { keyId = 21, code = 89, display = "Y", keypress = None }
        , { keyId = 22, code = 85, display = "U", keypress = None }
        , { keyId = 23, code = 73, display = "I", keypress = None }
        , { keyId = 24, code = 79, display = "O", keypress = None }
        , { keyId = 25, code = 80, display = "P", keypress = None }
        , { keyId = 26, code = 219, display = "[", keypress = None }
        , { keyId = 27, code = 221, display = "]", keypress = None }
        , { keyId = 28, code = 220, display = "\\", keypress = None }
        , { keyId = 29, code = 20, display = "Caps", keypress = None }
        , { keyId = 30, code = 65, display = "A", keypress = None }
        , { keyId = 31, code = 83, display = "S", keypress = None }
        , { keyId = 32, code = 68, display = "D", keypress = None }
        , { keyId = 33, code = 70, display = "F", keypress = None }
        , { keyId = 34, code = 71, display = "G", keypress = None }
        , { keyId = 35, code = 72, display = "H", keypress = None }
        , { keyId = 36, code = 74, display = "J", keypress = None }
        , { keyId = 37, code = 75, display = "K", keypress = None }
        , { keyId = 38, code = 76, display = "L", keypress = None }
        , { keyId = 39, code = 59, display = ";", keypress = None }
        , { keyId = 40, code = 222, display = "'", keypress = None }
        , { keyId = 41, code = 13, display = "Enter", keypress = None }
        , { keyId = 42, code = 16, display = "Shift", keypress = None }
        , { keyId = 43, code = 90, display = "Z", keypress = None }
        , { keyId = 44, code = 88, display = "X", keypress = None }
        , { keyId = 45, code = 67, display = "C", keypress = None }
        , { keyId = 46, code = 86, display = "V", keypress = None }
        , { keyId = 47, code = 66, display = "B", keypress = None }
        , { keyId = 48, code = 78, display = "N", keypress = None }
        , { keyId = 49, code = 77, display = "M", keypress = None }
        , { keyId = 50, code = 188, display = ",", keypress = None }
        , { keyId = 51, code = 190, display = ".", keypress = None }
        , { keyId = 52, code = 191, display = "/", keypress = None }
        , { keyId = 53, code = 16, display = "Shift", keypress = None }
        , { keyId = 54, code = 17, display = "Ctrl", keypress = None }
        , { keyId = 55, code = 224, display = "Cmd", keypress = None }
        , { keyId = 56, code = 18, display = "Alt", keypress = None }
        , { keyId = 57, code = 32, display = "Space", keypress = None }
        , { keyId = 58, code = 18, display = "Alt ", keypress = None }
        , { keyId = 59, code = 93, display = "Menu Key", keypress = None }
        , { keyId = 60, code = 17, display = "Ctrl", keypress = None }
        , { keyId = 61, code = 45, display = "Insert", keypress = None }
        , { keyId = 62, code = 46, display = "Delete", keypress = None }
        , { keyId = 63, code = 36, display = "Home", keypress = None }
        , { keyId = 64, code = 35, display = "End", keypress = None }
        , { keyId = 65, code = 33, display = "PgUp", keypress = None }
        , { keyId = 66, code = 34, display = "PgDown", keypress = None }
        , { keyId = 67, code = 38, display = "↑", keypress = None }
        , { keyId = 68, code = 37, display = "←", keypress = None }
        , { keyId = 69, code = 40, display = "↓", keypress = None }
        , { keyId = 70, code = 39, display = "→", keypress = None }
        , { keyId = 71, code = 111, display = "/", keypress = None }
        , { keyId = 72, code = 106, display = "*", keypress = None }
        , { keyId = 73, code = 109, display = "-", keypress = None }
        , { keyId = 74, code = 103, display = "7", keypress = None }
        , { keyId = 75, code = 104, display = "8", keypress = None }
        , { keyId = 76, code = 105, display = "9", keypress = None }
        , { keyId = 77, code = 107, display = "+", keypress = None }
        , { keyId = 78, code = 100, display = "4", keypress = None }
        , { keyId = 79, code = 101, display = "5", keypress = None }
        , { keyId = 80, code = 102, display = "6", keypress = None }
        , { keyId = 81, code = 97, display = "1", keypress = None }
        , { keyId = 82, code = 98, display = "2", keypress = None }
        , { keyId = 83, code = 99, display = "3", keypress = None }
        , { keyId = 84, code = 96, display = "0", keypress = None }
        , { keyId = 85, code = 108, display = ".", keypress = None }
        , { keyId = 86, code = 13, display = "Enter", keypress = None }
        ]
    }



-- keycodes : Dict Int String
-- keycodes =
--     Dict.fromList
--         [ ( 8, "backspace" )
--         , ( 9, "tab" )
--         , ( 13, "enter" )
--         , ( 16, "shift" )
--         , ( 17, "ctrl" )
--         , ( 18, "alt" )
--         , ( 19, "pause/break" )
--         , ( 20, "caps lock" )
--         , ( 27, "escape" )
--         , ( 32, "Space" )
--         , ( 33, "page up" )
--         , ( 34, "page down" )
--         , ( 35, "end" )
--         , ( 36, "home" )
--         , ( 37, "←" )
--         , ( 38, "↑" )
--         , ( 39, "→" )
--         , ( 40, "↓" )
--         , ( 42, "*" )
--         , ( 43, "+" )
--         , ( 45, "-" )
--         , ( 46, "." )
--         , ( 47, "/" )
--         , ( 48, "0" )
--         , ( 49, "1" )
--         , ( 50, "2" )
--         , ( 51, "3" )
--         , ( 52, "4" )
--         , ( 53, "5" )
--         , ( 54, "6" )
--         , ( 55, "7" )
--         , ( 56, "8" )
--         , ( 57, "9" )
--         , ( 65, "A" )
--         , ( 66, "B" )
--         , ( 67, "C" )
--         , ( 68, "D" )
--         , ( 69, "E" )
--         , ( 70, "F" )
--         , ( 71, "G" )
--         , ( 72, "H" )
--         , ( 73, "I" )
--         , ( 74, "J" )
--         , ( 75, "K" )
--         , ( 76, "L" )
--         , ( 77, "M" )
--         , ( 78, "N" )
--         , ( 79, "O" )
--         , ( 80, "P" )
--         , ( 81, "Q" )
--         , ( 82, "R" )
--         , ( 83, "S" )
--         , ( 84, "T" )
--         , ( 85, "U" )
--         , ( 86, "V" )
--         , ( 87, "W" )
--         , ( 88, "X" )
--         , ( 89, "Y" )
--         , ( 90, "Z" )
--         , ( 91, "left window key" )
--         , ( 92, "right window key" )
--         , ( 93, "select key" )
--         , ( 96, "numpad 0" )
--         , ( 97, "numpad 1" )
--         , ( 98, "numpad 2" )
--         , ( 99, "numpad 3" )
--         , ( 100, "numpad 4" )
--         , ( 101, "numpad 5" )
--         , ( 102, "numpad 6" )
--         , ( 103, "numpad 7" )
--         , ( 104, "numpad 8" )
--         , ( 105, "numpad 9" )
--         , ( 106, "multiply" )
--         , ( 107, "add" )
--         , ( 109, "subtract" )
--         , ( 110, "decimal point" )
--         , ( 111, "divide" )
--         , ( 112, "f1" )
--         , ( 113, "f2" )
--         , ( 114, "f3" )
--         , ( 115, "f4" )
--         , ( 116, "f5" )
--         , ( 117, "f6" )
--         , ( 118, "f7" )
--         , ( 119, "f8" )
--         , ( 120, "f9" )
--         , ( 121, "f10" )
--         , ( 122, "f11" )
--         , ( 123, "f12" )
--         , ( 144, "num lock" )
--         , ( 145, "scroll lock" )
--         , ( 186, ";" )
--         , ( 187, "=" )
--         , ( 188, "," )
--         , ( 189, "-" )
--         , ( 190, "." )
--         , ( 191, "/" )
--         , ( 192, "`" )
--         , ( 219, "[" )
--         , ( 220, "\\" )
--         , ( 221, "]" )
--         , ( 222, "'" )
--         ]
