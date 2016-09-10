module Keyboard.KeyboardLayouts exposing (..)

import Keyboard.Types exposing (..)
import Dict exposing (..)


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


keytext : Dict Int String
keytext =
    Dict.fromList
        [ ( 13, "Enter" )
        , ( 32, "Space" )
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
