module Keyboard.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard.Types exposing (..)
import Array exposing (..)
import String
import Json.Decode as Json
import SWEditor.Display exposing (..)
import SWEditor.EditorSign exposing (..)
import Keyboard.Shared exposing (..)
import SWEditor.Types exposing (Msg)
import MainChooser.Types exposing (Msg)


--import SubFeature.View exposing (root)


root :
    Model
    -> List (Keyboard.Shared.KeyConfig SWEditor.Types.Msg)
    -> { generalchooserkeyboard :
            List (Keyboard.Shared.KeyConfig MainChooser.Types.Msg)
       , groupchooserkeyboard :
            List (Keyboard.Shared.KeyConfig MainChooser.Types.Msg)
       , symbolchooserkeyboard :
            List (Keyboard.Shared.KeyConfig MainChooser.Types.Msg)
       }
    -> Int
    -> Html Keyboard.Types.Msg
root model signviewkeyboard chooserskeyboard footerwidth =
    let
        -- keyboarddisplay =
        --     case model.keyboardmode of
        --         SignView ->
        --             signviewkeyboard
        --         GeneralChooser ->
        --             chooserskeyboard.generalchooserkeyboard
        --         GroupChooser ->
        --             chooserskeyboard.groupchooserkeyboard
        --         SymbolChooser ->
        --             chooserskeyboard.symbolchooserkeyboard
        text1 =
            text "text1"
    in
        div [ class "keyboard" ]
            [ text (String.concat model.keyboardhistory)
            , div
                [ class "alphabetic", style [ ( "width", "66%" ) ] ]
                [ row model (List.range 1 14) text1 footerwidth
                , row model (List.range 15 28) text1 footerwidth
                , row model (List.range 29 41) text1 footerwidth
                , row model (List.range 42 53) text1 footerwidth
                , row model (List.range 54 60) text1 footerwidth
                ]
            , div
                [ class "arrows", style [ ( "width", "14%" ) ] ]
                [ div [ style [ ( "height", "40%" ), ( "margin-bottom", "15.5%" ) ] ]
                    [ row model (List.range 61 63) text1 footerwidth
                    , row model (List.range 64 66) text1 footerwidth
                    ]
                , div [ style [ ( "height", "40%" ) ] ]
                    [ row model (List.range 67 67) text1 footerwidth
                    , row model (List.range 68 70) text1 footerwidth
                    ]
                ]
            , div
                [ class "numeric", style [ ( "width", "20%" ) ] ]
                [ row model (List.range 71 73) text1 footerwidth
                , row model (List.range 74 77) text1 footerwidth
                , row model (List.range 78 80) text1 footerwidth
                , row model (List.range 81 83) text1 footerwidth
                , row model (List.range 84 86) text1 footerwidth
                ]
            ]


row : Model -> List Int -> Html Keyboard.Types.Msg -> Int -> Html Keyboard.Types.Msg
row model nums display footerwidth =
    div [ class "row" ]
        (createkeys model nums display footerwidth)


createkeys : Model -> List Int -> Html Keyboard.Types.Msg -> Int -> List (Html Keyboard.Types.Msg)
createkeys model nums display footerwidth =
    List.map (\n -> nkey model n display footerwidth) nums


nkey : Model -> Int -> Html Keyboard.Types.Msg -> Int -> Html Keyboard.Types.Msg
nkey model n display footerwidth =
    let
        leftmargin =
            17

        size =
            { height = 20, width = 20 }

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
                [ display
                  -- , Html.map
                  --     Display
                  --     (SWEditor.Display.scaledSignView display scale leftmargin)
                ]
            , span [] [ text <| getKeyDisplay n model ]
            ]


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
