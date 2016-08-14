module View exposing (root)

import Html exposing (..)
import Html.App as App exposing (..)
import Types exposing (..)
import Overlay.View


-- import Feature.View exposing (root)


root : Model -> Html Msg
root model =
    App.map Overlay (Overlay.View.root model.overlay)
