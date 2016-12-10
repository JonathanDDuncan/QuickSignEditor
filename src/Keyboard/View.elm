module Keyboard.View exposing (root)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard.Types exposing (..)
import Array exposing (..)
import String
import Json.Decode as Json
import SWEditor.Display exposing (..)
import SWEditor.EditorSign exposing (..)


--import SubFeature.View exposing (root)


root : Model -> EditorSign -> Int -> Html Msg
root model sign footerwidth =
    div [ class "keyboard" ]
        [ text (String.concat model.keyboardhistory)
        , div
            [ class "alphabetic", style [ ( "width", "66%" ) ] ]
            [ row model [1..14] sign footerwidth
            , row model [15..28] sign footerwidth
            , row model [29..41] sign footerwidth
            , row model [42..53] sign footerwidth
            , row model [54..60] sign footerwidth
            ]
        , div
            [ class "arrows", style [ ( "width", "14%" ) ] ]
            [ div [ style [ ( "height", "40%" ), ( "margin-bottom", "15.5%" ) ] ]
                [ row model [61..63] sign footerwidth
                , row model [64..66] sign footerwidth
                ]
            , div [ style [ ( "height", "40%" ) ] ]
                [ row model [67..67] sign footerwidth
                , row model [68..70] sign footerwidth
                ]
            ]
        , div
            [ class "numeric", style [ ( "width", "20%" ) ] ]
            [ row model [71..73] sign footerwidth
            , row model [74..77] sign footerwidth
            , row model [78..80] sign footerwidth
            , row model [81..83] sign footerwidth
            , row model [84..86] sign footerwidth
            ]
        ]


row : Model -> List Int -> EditorSign -> Int -> Html Msg
row model nums sign footerwidth =
    div [ class "row" ]
        (createkeys model nums sign footerwidth)


createkeys : Model -> List Int -> EditorSign -> Int -> List (Html Msg)
createkeys model nums sign footerwidth =
    List.map (\n -> nkey model n sign footerwidth) nums


nkey : Model -> Int -> EditorSign -> Int -> Html Msg
nkey model n sign footerwidth =
    let
        leftmargin =
            17

        scale =
            calcscale sign 30 ((minkeywidth footerwidth) - leftmargin)

        width =
            round (toFloat sign.width * scale)

        ispressed =
            checkifkeypressed model n

        pressed =
            if (ispressed) then
                " pressed"
            else
                ""
    in
        div [ class <| "key k" ++ toString n , onClick (KeyClicked n), onTouchStart (KeyClicked n) ]
            [ div [ class  <| "scaletoparent" ++ pressed ]
                [ App.map
                    Display
                    (SWEditor.Display.scaledSignView sign scale leftmargin)
                ]
            , span [] [ text <| getKeyDisplay n model ]
            ]


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
            Maybe.withDefault { code = 0, display = "", keypress = None } (nth n model.keyboardlayout.keys)
    in
        keycode.display


nth : Int -> List a -> Maybe a
nth i list =
    Array.get i (Array.fromList list)


onTouchStart : msg -> Attribute msg
onTouchStart message =
    on "touchstart" (Json.succeed message)
