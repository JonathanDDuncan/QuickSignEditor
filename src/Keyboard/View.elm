module Keyboard.View exposing (root)

import Html exposing (Html, Attribute, div, text, span)
import Html.Attributes exposing (style, class)
import Html.Events exposing (on)
import Keyboard.Types exposing (Model, Msg(..), Keypress(..))
import Array exposing (..)
import String
import Json.Decode as Json
import Keyboard.Shared exposing (..)
import SWEditor.Types exposing (Msg)
import Choosers.Types exposing (Msg, ChoosersKeyboard)
import Helpers.ViewExtra exposing (..)


--import SubFeature.View exposing (root)


root :
    Model
    -> List (Keyboard.Shared.KeyAction SWEditor.Types.Msg)
    -> ChoosersKeyboard
    -> Int
    -> Int
    -> Html Keyboard.Types.Msg
root model signviewkeyboard chooserskeyboard keyboardwidth keyboardheight =
    let
        keyboardcommand =
            createKeyboardCommand model.keyList model.keyboardmode

        keyboarddisplay =
            case model.keyboardmode of
                SignView ->
                    convertotkeyboardmsg keyboardcommand DisplaySignView signviewkeyboard

                GeneralChooser ->
                    convertotkeyboardmsg keyboardcommand DisplayChoosers chooserskeyboard.generalchooserkeyboard

                GroupChooser ->
                    convertotkeyboardmsg keyboardcommand DisplayChoosers chooserskeyboard.groupchooserkeyboard

                SymbolChooser ->
                    convertotkeyboardmsg keyboardcommand DisplayChoosers chooserskeyboard.symbolchooserkeyboard

        rowheight =
            toFloat keyboardheight / 5
    in
        div [ class "keyboard" ]
            [ text (String.concat model.keyboardhistory)
            , div
                [ class "alphabetic", style [ ( "width", "66%" ) ] ]
                (List.map (row model keyboarddisplay keyboardwidth rowheight)
                    [ List.range 1 14
                    , List.range 15 28
                    , List.range 29 41
                    , List.range 42 53
                    , List.range 54 60
                    ]
                )
            , div
                [ class "arrows", style [ ( "width", "14%" ) ] ]
                [ div [ style [ ( "height", "40%" ), ( "margin-bottom", "15.5%" ) ] ]
                    (List.map (row model keyboarddisplay keyboardwidth rowheight)
                        [ List.range 61 63
                        , List.range 64 66
                        ]
                    )
                , div [ style [ ( "height", "40%" ) ] ]
                    (List.map (row model keyboarddisplay keyboardwidth rowheight)
                        [ List.range 67 67
                        , List.range 68 70
                        ]
                    )
                ]
            , div
                [ class "numeric", style [ ( "width", "20%" ) ] ]
                (List.map (row model keyboarddisplay keyboardwidth rowheight)
                    [ List.range 71 73
                    , List.range 74 77
                    , List.range 78 80
                    , List.range 81 83
                    , List.range 84 86
                    ]
                )
            ]


convertotkeyboardmsg :
    KeyboardCommand
    -> (msg -> msg1)
    -> List (KeyAction msg)
    -> List (KeyConfig msg1)
convertotkeyboardmsg keyboardcommand newmsg keyboarddisplay =
    List.map
        (\config ->
            { display =
                { width = config.display.width
                , height = config.display.height
                , backgroundcolor = config.display.backgroundcolor
                , view = Html.map newmsg config.display.view
                }
            , test = config.test
            }
        )
        (filtereddisplay
            keyboardcommand
            keyboarddisplay
        )


filtereddisplay :
    KeyboardCommand
    -> List (KeyAction msg)
    -> List (KeyAction msg)
filtereddisplay keyboardcommand keyboarddisplay =
    List.filter
        (\disp ->
            disp.test.ctrl
                == keyboardcommand.ctrlPressed
                && disp.test.shift
                == keyboardcommand.shiftPressed
                && disp.test.alt
                == keyboardcommand.altPressed
        )
        keyboarddisplay


row : Model -> List (KeyConfig Keyboard.Types.Msg) -> Int -> Float -> List Int -> Html Keyboard.Types.Msg
row model display keyboardwidth rowheight nums =
    div [ class "row" ]
        (createkeys model nums display keyboardwidth rowheight)


createkeys : Model -> List Int -> List (KeyConfig Keyboard.Types.Msg) -> Int -> Float -> List (Html Keyboard.Types.Msg)
createkeys model nums display keyboardwidth rowheight =
    List.map (\n -> nkey model n display keyboardwidth rowheight) nums


nkey : Model -> Int -> List (KeyConfig Keyboard.Types.Msg) -> Int -> Float -> Html Keyboard.Types.Msg
nkey model n displays keyboardwidth rowheight =
    let
        leftmargin =
            10

        display =
            (getkeydisplay n displays).display

        originalsize =
            { height = display.height, width = display.width }

        backgroundcolor =
            display.backgroundcolor

        finalheight =
            rowheight - 5.0

        finalwidth =
            minkeywidth keyboardwidth - leftmargin

        scale =
            shrinkdontzoom (toFloat originalsize.width) (toFloat originalsize.height) finalwidth finalheight

        ispressed =
            checkifkeypressed model n

        pressed =
            if ispressed then
                " pressed"
            else
                ""

        activemode =
            if isactivemode n model then
                " activemode"
            else
                ""
    in
        div
            [ class <| "key k" ++ toString n ++ pressed ++ activemode
            , style
                [ case display.backgroundcolor of
                    Just color ->
                        ( "background-color", color )

                    Nothing ->
                        ( "", "" )
                ]
            ]
            [ div [ class " centerdivcontainer" ]
                [ div
                    [ style
                        [ ( "transform-origin", "top left" )
                        , transformscale scale
                        , ( "margin-left", px leftmargin )
                        , ( "width", px <| round <| toFloat originalsize.width * scale )
                        , ( "height", px <| round <| toFloat originalsize.height * scale )
                        ]
                    , class "centerdiv"
                    ]
                    [ display.view ]
                ]
            , span [] [ text <| getKeyDisplay n model ]
            ]


getkeydisplay : Int -> List (KeyConfig msg) -> KeyConfig msg
getkeydisplay n display =
    List.filter (\disp -> disp.test.key == n) display
        |> List.head
        |> Maybe.withDefault
            { display = { width = 20, height = 20, backgroundcolor = Nothing, view = text "" }
            , test = { key = 0, ctrl = False, shift = False, alt = True }
            }


isactivemode : number -> { a | keyboardmode : KeyboardMode } -> Bool
isactivemode n model =
    case model.keyboardmode of
        GeneralChooser ->
            n == 2

        GroupChooser ->
            n == 3

        SymbolChooser ->
            n == 4

        SignView ->
            n == 5


checkifkeypressed : Model -> Int -> Bool
checkifkeypressed model n =
    List.any ((==) n) model.keyList


minkeywidth : Int -> Float
minkeywidth keyboardwidth =
    toFloat keyboardwidth * 0.66 * 0.063 - 3


getKeyDisplay : Int -> Model -> String
getKeyDisplay n model =
    let
        keycode =
            Maybe.withDefault { keyId = 0, code = 0, display = "", keypress = None } (nth n model.keyboardlayout.keys)
    in
        keycode.display


nth : Int -> List a -> Maybe a
nth i list =
    Array.get i (Array.fromList list)



-- onTouchStart : msg -> Attribute msg
-- onTouchStart message =
--     on "touchstart" (Json.succeed message)
-- onTouchEnd : msg -> Attribute msg
-- onTouchEnd message =
--     on "touchend" (Json.succeed message)
