module Keyboard.Shared exposing (..)


type KeyboardMode
    = SignView
    | GeneralChooser
    | GroupChooser
    | SymbolChooser


keyboardModeCode : List ( Int, KeyboardMode )
keyboardModeCode =
    [ ( 1, SignView )
    , ( 2, GeneralChooser )
    , ( 3, GroupChooser )
    , ( 4, SymbolChooser )
    ]


getKeyboardMode : Int -> KeyboardMode
getKeyboardMode value =
    List.filter (\m -> fst m == value) keyboardModeCode
        |> List.map (\m -> snd m)
        |> List.head
        |> Maybe.withDefault SignView


type alias KeyboardCommand =
    { mode : Int
    , shiftPressed : Bool
    , ctrlPressed : Bool
    , altPressed : Bool
    , keys : List Int
    }
