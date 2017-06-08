module View exposing (root)

import Html exposing (Html)
import Types exposing (Model, Msg(..))
import Overlay.View


root : Model -> Html Msg
root model =
    Html.map Overlay (Overlay.View.root model.overlay)
