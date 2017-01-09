module View exposing (root)

import Html exposing (..)
import Types exposing (..)
import Overlay.View


-- import Feature.View exposing (root)


root : Model -> Html Msg
root model =
    Html.map Overlay (Overlay.View.root model.overlay)
