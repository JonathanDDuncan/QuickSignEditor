module Feature.View exposing (root)

import Html exposing (..)
import Feature.Types exposing (..)


-- You may need these latter
-- import Html.Events exposing (..)
-- import Html.Attributes exposing (..)
-- import Set
-- import String
-- import feature types and sub views
-- import Widget.Types
-- import Widget.View
-- import Grommit.Types
-- import Grommit.View


root : Model -> Html Msg
root model =
    div []
        [ text "This is where the views go"
        ]
