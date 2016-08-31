module Keyboard.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard.Types exposing (..)
import Array exposing (..)


--import SubFeature.View exposing (root)


root : Model -> Html Msg
root model =
    div [ class "keyboard" ]
        [ row model [1..14]
        , row model [15..28]
        , row model [29..41]
        , row model [42..55]
        , row model [56..63]
        ]


row : Model -> List Int -> Html Msg
row model nums =
    div []
        (createkeys model nums)


nkey : Model -> Int -> Html Msg
nkey model n =
    div []
        [ span []
            [ text <| toString (getKeyCode n model) ]
        , span []
            [ text "X" ]
        ]


getKeyCode : Int -> Model -> Int
getKeyCode n model =
    let
        keycode =
            nth n model.keyboardlayout.codes
    in
        Maybe.withDefault 0 keycode


nth : Int -> List a -> Maybe a
nth i list =
    Array.get i (Array.fromList list)


createkeys : Model -> List Int -> List (Html Msg)
createkeys model nums =
    List.map (\n -> nkey model n) nums
