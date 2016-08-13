module View exposing (root)

import Html exposing (..)
import Types exposing (..)
-- import Feature.View exposing (root)

root : Model -> Html Msg
root model =
    div []
        [ text "This is text"
        -- Nest another view by adding
        -- App.map FeatureMsg (Feature.View.root model.featureFieldName)
        ]
