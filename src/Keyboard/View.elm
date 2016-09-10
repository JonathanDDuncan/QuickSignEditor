module Keyboard.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard.Types exposing (..)
import Array exposing (..)
import String
import Json.Decode as Json


--import SubFeature.View exposing (root)


root : Model -> Html Msg
root model =
    div [ class "keyboard" ]
        [ text (String.concat model.keyboardhistory)
        , div
            [ class "alphabetic", style [ ( "width", "66%" ) ] ]
            [ row model [1..14]
            , row model [15..28]
            , row model [29..41]
            , row model [42..53]
            , row model [54..60]
            ]
        , div
            [ class "arrows", style [ ( "width", "14%" ) ] ]
            [ div [ style [ ( "height", "40%" ), ( "margin-bottom", "15.5%" ) ] ]
                [ row model [61..63]
                , row model [64..66]
                ]
            , div [ style [ ( "height", "40%" ) ] ]
                [ row model [67..67]
                , row model [68..70]
                ]
            ]
        , div
            [ class "numeric", style [ ( "width", "20%" ) ] ]
            [ row model [71..73]
            , row model [74..77]
            , row model [78..80]
            , row model [81..83]
            , row model [84..86]
            ]
        ]


row : Model -> List Int -> Html Msg
row model nums =
    div [ class "row" ]
        (createkeys model nums)


nkey : Model -> Int -> Html Msg
nkey model n =
    div [ class "key", onClick (KeyClicked n), onTouchStart (KeyClicked n) ]
        [ span []
            []
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


createkeys : Model -> List Int -> List (Html Msg)
createkeys model nums =
    List.map (\n -> nkey model n) nums


onTouchStart : msg -> Attribute msg
onTouchStart message =
    on "touchstart" (Json.succeed message)
