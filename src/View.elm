module View exposing (root)

import Html exposing (..)
import Types exposing (..)


root : Model -> Html Msg
root model =
    div []
        [ text "This is text"
        ]
