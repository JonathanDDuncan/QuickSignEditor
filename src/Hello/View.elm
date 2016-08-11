module Hello.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String
import Hello.Types  exposing (..)


root : Model -> Html Msg
root model =
    div []
        [ div
            [ class "h1" ]
            [ text ("Hello, Elm" ++ ("!" |> String.repeat model.counter)) ]
        , p [] [ text ("Elm Webpack Starter") ]
        , button
            [ class "btn btn-primary btn-lg"
            , onClick Hello.Types.Increment
            ]
            [ -- click handler
              span [ class "glyphicon glyphicon-star" ] []
              -- glyphicon
            , span [] [ text "FTW!" ]
            ]
        ]
