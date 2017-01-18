module Keyboard.Shared exposing (..)

import Update.Extra exposing (..)


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
    List.filter (\m -> Tuple.first m == value) keyboardModeCode
        |> List.map (\m -> Tuple.second m)
        |> List.head
        |> Maybe.withDefault SignView


runKeyboard :
    a
    -> KeyboardCommand
    -> (msg -> a -> ( a, Cmd msg ))
    -> List (KeyConfig msg)
    -> ( a, Cmd msg )
runKeyboard model command update config =
    List.foldl (\item -> runkeycommand command update item) (model ! []) config


runkeycommand :
    KeyboardCommand
    -> (a -> model -> ( model, Cmd a ))
    -> KeyConfig a
    -> ( model, Cmd a )
    -> ( model, Cmd a )
runkeycommand command update config =
    Update.Extra.filter (runtest command config.test)
        (Update.Extra.andThen update <| config.action)


runtest : KeyboardCommand -> KeyTestConfig -> Bool
runtest command test =
    (pressed command test.key) && List.all (\spec -> spec command) test.special


pressed : KeyboardCommand -> Int -> Bool
pressed command val =
    List.any ((==) val) command.keys


type alias KeyConfig a =
    { test : KeyTestConfig
    , action : a
    }


type alias KeyTestConfig =
    { key : Int
    , special : List (KeyboardCommand -> Bool)
    }


type alias KeyboardCommand =
    { mode : Int
    , shiftPressed : Bool
    , ctrlPressed : Bool
    , altPressed : Bool
    , keys : List Int
    }
