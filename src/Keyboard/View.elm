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
import SW.Types exposing (..)


--import SubFeature.View exposing (root)


root : Model -> EditorSign -> Html Msg
root model sign =
    div [ class "keyboard" ]
        [ text (String.concat model.keyboardhistory)
        , div
            [ class "alphabetic", style [ ( "width", "66%" ) ] ]
            [ row model [1..14] sign
            , row model [15..28] sign
            , row model [29..41] sign
            , row model [42..53] sign
            , row model [54..60] sign
            ]
        , div
            [ class "arrows", style [ ( "width", "14%" ) ] ]
            [ div [ style [ ( "height", "40%" ), ( "margin-bottom", "15.5%" ) ] ]
                [ row model [61..63] sign
                , row model [64..66] sign
                ]
            , div [ style [ ( "height", "40%" ) ] ]
                [ row model [67..67] sign
                , row model [68..70] sign
                ]
            ]
        , div
            [ class "numeric", style [ ( "width", "20%" ) ] ]
            [ row model [71..73] sign
            , row model [74..77] sign
            , row model [78..80] sign
            , row model [81..83] sign
            , row model [84..86] sign
            ]
        ]


row : Model -> List Int -> EditorSign -> Html Msg
row model nums sign =
    div [ class "row" ]
        (createkeys model nums sign)


createkeys : Model -> List Int -> EditorSign -> List (Html Msg)
createkeys model nums sign =
    List.map (\n -> nkey model n sign) nums


nkey : Model -> Int -> EditorSign -> Html Msg
nkey model n sign =
    div [ class "key", onClick (KeyClicked n), onTouchStart (KeyClicked n) ]
        [ App.map
            Display
            (SWEditor.Display.signView sign [ class "scaletoparent" ])
        , span []
            [ text <| getKeyDisplay n model ]
        ]


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
