module Choosers.GeneralChooserKeyboard exposing (runKeyboardCommand, creategeneralchooserkeyboard)

import Choosers.Types exposing (Model, ChoosingModel, Msg(..), Editor(..))
import Html
import Keyboard.Shared exposing (KeyboardCommand, KeyAction, KeyboardMode(..), getKeyboardMode, runKeyboard)
import SWEditor.DisplaySvg exposing (signdisplaysvg, signsvg)


runKeyboardCommand :
    Choosers.Types.Model
    -> KeyboardCommand
    -> (Msg -> Choosers.Types.Model -> ( Choosers.Types.Model, Cmd Msg ))
    -> ( Choosers.Types.Model, Cmd Msg )
runKeyboardCommand model command update =
    let
        mode =
            getKeyboardMode command.mode

        updatetuple =
            if mode == GeneralChooser then
                runKeyboard model command update model.chooserskeyboard.generalchooserkeyboard
            else if mode == GroupChooser then
                runKeyboard model command update model.chooserskeyboard.groupchooserkeyboard
            else if mode == SymbolChooser then
                runKeyboard model command update model.chooserskeyboard.symbolchooserkeyboard
            else
                model ! []
    in
        updatetuple


creategeneralchooserkeyboard :
    List ChoosingModel
    -> List (KeyAction Msg)
creategeneralchooserkeyboard choosings =
    List.indexedMap
        (\i choosing ->
            let
                layoutsetting =
                    getlayoutsettings i layout
            in
                { test = { key = layoutsetting.key, ctrl = False, shift = False, alt = False }
                , action = ((Editor << Clicked) choosing.value)
                , display =
                    { width = choosing.displaySign.width
                    , height = choosing.displaySign.height
                    , backgroundcolor = Nothing
                    , view =
                        Html.map Choosers.Types.SignView
                            (signsvg choosing.displaySign)
                    }
                }
        )
        (choosings)


getlayoutsettings :
    Int
    -> List { index : Int, key : Int, overrideheight : Maybe Int, toppadding : Int }
    -> { index : Int, key : Int, overrideheight : Maybe Int, toppadding : Int }
getlayoutsettings n layout =
    List.filter (\l -> l.index == n) layout
        |> List.head
        |> Maybe.withDefault { index = 0, key = 0, overrideheight = Nothing, toppadding = 5 }


layout : List { index : Int, key : Int, overrideheight : Maybe Int, toppadding : Int }
layout =
    [ { index = 0, key = 3, overrideheight = Just 50, toppadding = 5 }
    , { index = 1, key = 2, overrideheight = Just 50, toppadding = 5 }
    , { index = 2, key = 4, overrideheight = Just 20, toppadding = 5 }
    , { index = 3, key = 0, overrideheight = Nothing, toppadding = 5 }
    , { index = 4, key = 5, overrideheight = Just 20, toppadding = 15 }
    , { index = 5, key = 7, overrideheight = Just 20, toppadding = 20 }
    , { index = 6, key = 6, overrideheight = Just 20, toppadding = 8 }
    , { index = 7, key = 35, overrideheight = Just 55, toppadding = 8 }
    , { index = 8, key = 8, overrideheight = Just 20, toppadding = 12 }
    , { index = 9, key = 0, overrideheight = Nothing, toppadding = 5 }
    , { index = 10, key = 21, overrideheight = Just 60, toppadding = 8 }
    , { index = 11, key = 33, overrideheight = Nothing, toppadding = 5 }
    , { index = 12, key = 30, overrideheight = Just 20, toppadding = 3 }
    , { index = 13, key = 31, overrideheight = Just 25, toppadding = 5 }
    , { index = 14, key = 48, overrideheight = Just 25, toppadding = 5 }
    , { index = 15, key = 32, overrideheight = Just 20, toppadding = 5 }
    , { index = 16, key = 19, overrideheight = Just 20, toppadding = 8 }
    , { index = 17, key = 20, overrideheight = Nothing, toppadding = 5 }
    , { index = 18, key = 18, overrideheight = Just 20, toppadding = 5 }
    , { index = 19, key = 17, overrideheight = Nothing, toppadding = 5 }
    , { index = 20, key = 16, overrideheight = Nothing, toppadding = 5 }
    , { index = 21, key = 46, overrideheight = Nothing, toppadding = 5 }
    , { index = 22, key = 45, overrideheight = Nothing, toppadding = 5 }
    , { index = 23, key = 43, overrideheight = Nothing, toppadding = 5 }
    , { index = 24, key = 34, overrideheight = Nothing, toppadding = 5 }
    , { index = 25, key = 47, overrideheight = Just 20, toppadding = 5 }
    , { index = 26, key = 9, overrideheight = Just 20, toppadding = 5 }
    , { index = 27, key = 50, overrideheight = Just 20, toppadding = 8 }
    , { index = 28, key = 51, overrideheight = Just 30, toppadding = 1 }
    , { index = 29, key = 52, overrideheight = Just 40, toppadding = 1 }
    , { index = 30, key = 49, overrideheight = Just 40, toppadding = 1 }
    , { index = 31, key = 44, overrideheight = Just 20, toppadding = 5 }
    , { index = 32, key = 10, overrideheight = Nothing, toppadding = 5 }
    , { index = 33, key = 11, overrideheight = Just 20, toppadding = 8 }
    ]
