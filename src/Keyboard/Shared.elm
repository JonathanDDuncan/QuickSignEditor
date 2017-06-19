module Keyboard.Shared
    exposing
        ( KeyAction
        , KeyboardCommand
        , KeyConfig
        , getKeyboardMode
        , runKeyboard
        , createKeyboardCommand
        , getKeyboardModeCode
        , isPressedShift
        )

import Update.Extra exposing (andThen)
import Html exposing (Html)
import Keyboard.KeyboardModeType exposing (..)


keyboardModeCode : List ( Int, KeyboardMode )
keyboardModeCode =
    [ ( 1, GeneralChooser )
    , ( 2, GroupChooser )
    , ( 3, SymbolChooser )
    , ( 4, SignView )
    ]


getKeyboardMode : Int -> KeyboardMode
getKeyboardMode value =
    List.filter (\m -> Tuple.first m == value) keyboardModeCode
        |> List.map (\m -> Tuple.second m)
        |> List.head
        |> Maybe.withDefault GeneralChooser


getKeyboardModeCode : KeyboardMode -> Int
getKeyboardModeCode mode =
    keyboardModeCode
        |> List.filter (\m -> Tuple.second m == mode)
        |> List.map (\m -> Tuple.first m)
        |> List.head
        |> Maybe.withDefault 1


runKeyboard :
    a
    -> KeyboardCommand
    -> (msg -> a -> ( a, Cmd msg ))
    -> List (KeyAction msg)
    -> ( a, Cmd msg )
runKeyboard model command update config =
    List.filter (\item -> runtest command item.test) config
        |> List.foldl (\item -> runkeycommand update item) (model ! [])


runkeycommand :
    (a -> model -> ( model, Cmd a ))
    -> KeyAction a
    -> ( model, Cmd a )
    -> ( model, Cmd a )
runkeycommand update config =
    andThen update config.action


runtest : KeyboardCommand -> KeyTestConfig -> Bool
runtest command test =
    pressed command test.key
        && isPressedCtrl command.keys
        == test.ctrl
        && isPressedShift command.keys
        == test.shift
        && isPressedAlt command.keys
        == test.alt


pressed : KeyboardCommand -> Int -> Bool
pressed command val =
    List.any ((==) val) command.keys


createKeyboardCommand :
    List Int
    -> KeyboardMode
    -> KeyboardCommand
createKeyboardCommand keyList mode =
    { shiftPressed = isPressedShift keyList
    , ctrlPressed = isPressedCtrl keyList
    , altPressed = isPressedAlt keyList
    , mode = getKeyboardModeCode mode
    , keys = keyList
    }


isPressedShift : List Int -> Bool
isPressedShift keyList =
    List.any ((==) 42) keyList || List.any ((==) 53) keyList


isPressedCtrl : List Int -> Bool
isPressedCtrl keyList =
    List.any ((==) 54) keyList || List.any ((==) 60) keyList


isPressedAlt : List Int -> Bool
isPressedAlt keyList =
    List.any ((==) 56) keyList || List.any ((==) 58) keyList


type alias KeyAction a =
    { test : KeyTestConfig
    , action : a
    , display : KeyDisplay a
    }


type alias KeyDisplay a =
    { width : Int
    , height : Int
    , backgroundcolor : Maybe String
    , view : Html.Html a
    }


type alias KeyConfig a =
    { test : KeyTestConfig
    , display : KeyDisplay a
    }


type alias KeyTestConfig =
    { key : Int
    , ctrl : Bool
    , shift : Bool
    , alt : Bool
    }


type alias KeyboardCommand =
    { mode : Int
    , shiftPressed : Bool
    , ctrlPressed : Bool
    , altPressed : Bool
    , keys : List Int
    }
