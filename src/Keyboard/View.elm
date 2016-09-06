module Keyboard.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard.Types exposing (..)
import Array exposing (..)
import Dict exposing (..)


--import SubFeature.View exposing (root)


root : Model -> Html Msg
root model =
    div [ class "keyboard" ]
        [ div [ class "alphabetic", style [ ( "width", "66%" ) ] ]
            [ row model [1..14]
            , row model [15..28]
            , row model [29..41]
            , row model [42..53]
            , row model [54..60]
            ]
        , div
            [ class "arrows", style [ ( "width", "14%" ) ] ]
            [ div [ style [ ( "height", "40%" ), ( "margin-bottom", "17%" ) ] ]
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
            , row model [78..81]
            , row model [82..85]
            , row model [86..87]
            ]
        ]


row : Model -> List Int -> Html Msg
row model nums =
    div [ class "row" ]
        (createkeys model nums)


nkey : Model -> Int -> Html Msg
nkey model n =
    div [ class "key" ]
        [ span []
            []
        , span []
            [ text <| getKeyString n model ]
        ]


getKeyCode : Int -> Model -> Int
getKeyCode n model =
    let
        keycode =
            nth n model.keyboardlayout.codes
    in
        Maybe.withDefault 0 keycode


getKeyString : Int -> Model -> String
getKeyString n model =
    let
        keycode =
            Maybe.withDefault 0 (nth n model.keyboardlayout.codes)

        keystring =
            Dict.get keycode model.keycodedictionary
    in
        Maybe.withDefault (toString keycode) keystring


nth : Int -> List a -> Maybe a
nth i list =
    Array.get i (Array.fromList list)


createkeys : Model -> List Int -> List (Html Msg)
createkeys model nums =
    List.map (\n -> nkey model n) nums
