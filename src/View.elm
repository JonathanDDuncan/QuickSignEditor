module View exposing (root)

import Html exposing (Html)
import Types exposing (Model, Msg(..))
import Overlay.State


root : Model -> Html Msg
root model =
    Html.map Overlay (Overlay.State.root model.overlay)
