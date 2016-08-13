module Feature.View exposing (root)

import Html exposing (..)
import Feature.Types exposing (..)
--import SubFeature.View exposing (root)

root : Model -> Html Msg
root model =
    div []
        [ text "Put the view here"
        ]
