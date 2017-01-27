module Keyboard.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard.Types exposing (..)
import Array exposing (..)
import String
import Json.Decode as Json
import Keyboard.Shared exposing (..)
import SWEditor.Types exposing (Msg)
import MainChooser.Types exposing (Msg)
import ViewHelper.ViewExtra exposing (..)


--import SubFeature.View exposing (root)


root :
    Model
    -> List (Keyboard.Shared.KeyAction SWEditor.Types.Msg)
    -> { generalchooserkeyboard :
            List (Keyboard.Shared.KeyAction MainChooser.Types.Msg)
       , groupchooserkeyboard :
            List (Keyboard.Shared.KeyAction MainChooser.Types.Msg)
       , symbolchooserkeyboard :
            List (Keyboard.Shared.KeyAction MainChooser.Types.Msg)
       }
    -> Int
    -> Html Keyboard.Types.Msg
root model signviewkeyboard chooserskeyboard footerwidth =
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
    in
        div [ class "keyboard" ]
            [ text (String.concat model.keyboardhistory)
            , div
                [ class "alphabetic", style [ ( "width", "66%" ) ] ]
                [ row model (List.range 1 14) keyboarddisplay footerwidth
                , row model (List.range 15 28) keyboarddisplay footerwidth
                , row model (List.range 29 41) keyboarddisplay footerwidth
                , row model (List.range 42 53) keyboarddisplay footerwidth
                , row model (List.range 54 60) keyboarddisplay footerwidth
                ]
            , div
                [ class "arrows", style [ ( "width", "14%" ) ] ]
                [ div [ style [ ( "height", "40%" ), ( "margin-bottom", "15.5%" ) ] ]
                    [ row model (List.range 61 63) keyboarddisplay footerwidth
                    , row model (List.range 64 66) keyboarddisplay footerwidth
                    ]
                , div [ style [ ( "height", "40%" ) ] ]
                    [ row model (List.range 67 67) keyboarddisplay footerwidth
                    , row model (List.range 68 70) keyboarddisplay footerwidth
                    ]
                ]
            , div
                [ class "numeric", style [ ( "width", "20%" ) ] ]
                [ row model (List.range 71 73) keyboarddisplay footerwidth
                , row model (List.range 74 77) keyboarddisplay footerwidth
                , row model (List.range 78 80) keyboarddisplay footerwidth
                , row model (List.range 81 83) keyboarddisplay footerwidth
                , row model (List.range 84 86) keyboarddisplay footerwidth
                ]
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


row : Model -> List Int -> List (KeyConfig Keyboard.Types.Msg) -> Int -> Html Keyboard.Types.Msg
row model nums display footerwidth =
    div [ class "row" ]
        (createkeys model nums display footerwidth)


createkeys : Model -> List Int -> List (KeyConfig Keyboard.Types.Msg) -> Int -> List (Html Keyboard.Types.Msg)
createkeys model nums display footerwidth =
    List.map (\n -> nkey model n display footerwidth) nums


nkey : Model -> Int -> List (KeyConfig Keyboard.Types.Msg) -> Int -> Html Keyboard.Types.Msg
nkey model n display footerwidth =
    let
        leftmargin =
            17

        display1 =
            Debug.log "display1" (getkeydisplay n display).display

        size =
            { height = display1.height, width = display1.width }

        scale =
            calcscale size 30 ((minkeywidth footerwidth) - leftmargin)

        width =
            round (toFloat size.width * scale)

        ispressed =
            checkifkeypressed model n

        pressed =
            if (ispressed) then
                " pressed"
            else
                ""

        activemode =
            if (isactivemode n model) then
                " activemode"
            else
                ""
    in
        div [ class <| "key k" ++ toString n, onClick (KeyClicked n), onTouchEnd (KeyClicked n) ]
            [ div [ class <| "scaletoparent" ++ pressed ++ activemode ]
                [ div [ style [ ( "transform", ("scale(" ++ toString scale ++ ")") ), ( "margin-left", px leftmargin ) ] ] [ display1.view ]
                  -- , Html.map
                  --     Display
                  --     (SWEditor.Display.scaledSignView display scale leftmargin)
                ]
            , span [] [ text <| getKeyDisplay n model ]
            ]


getkeydisplay : Int -> List (KeyConfig msg) -> KeyConfig msg
getkeydisplay n display =
    List.filter (\disp -> disp.test.key == n) display
        |> List.head
        |> Maybe.withDefault
            { display = { width = 20, height = 20, view = text "" }
            , test = { key = 0, ctrl = False, shift = False, alt = True }
            }


isactivemode : number -> { a | keyboardmode : KeyboardMode } -> Bool
isactivemode n model =
    case model.keyboardmode of
        SignView ->
            n == 2

        GeneralChooser ->
            n == 3

        GroupChooser ->
            n == 4

        SymbolChooser ->
            n == 5


checkifkeypressed : Model -> Int -> Bool
checkifkeypressed model n =
    List.any ((==) n) model.keyList


minkeywidth : Int -> Float
minkeywidth footerwidth =
    toFloat footerwidth * 0.66 * 0.063 - 3


calcscale : { a | height : Int, width : Int } -> Float -> Float -> Float
calcscale sign height width =
    let
        availableWidth =
            width

        contentWidth =
            sign.width

        availableHeight =
            height

        contentHeight =
            sign.height

        scale =
            Basics.min (availableWidth / toFloat contentWidth) (availableHeight / toFloat contentHeight)
    in
        scale


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


onTouchStart : msg -> Attribute msg
onTouchStart message =
    on "touchstart" (Json.succeed message)


onTouchEnd : msg -> Attribute msg
onTouchEnd message =
    on "touchend" (Json.succeed message)
