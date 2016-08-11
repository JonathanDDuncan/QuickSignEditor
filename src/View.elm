module View exposing (root)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)
import Jumbotron.View


root : Model -> Html Msg
root model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "col-xs-12" ]
                [ App.map Jumbotron (Jumbotron.View.root model.jumbotron)
                ]
            ]
        ]
