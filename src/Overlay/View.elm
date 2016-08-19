module Overlay.View exposing (root)

import Html exposing (..)
import Overlay.Types exposing (..)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (..)
import Html.App as App
import Layout.View


root : Model -> Html Msg
root model =
    if model.show then
        div [ class "overlay" ]
            [ App.map Layout (Layout.View.root model.layout)
            ]
    else
        div []
            [ button [ onClick Show ] [ text "Show" ]
            ]
