module Overlay.View exposing (root)

import Html exposing (..)
import Overlay.Types exposing (..)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (..)
 
import Layout.View


root : Model -> Html Msg
root model =
    if model.show then
        div [ class "overlay" ]
            [ Html.map Layout (Layout.View.root model.layout)
            ]
    else
        div [ class "readytoshow" ]
            [ button [ onClick Show ] [ text "Quick" ]
            ]
