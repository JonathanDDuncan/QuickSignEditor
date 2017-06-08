module WindowSize.View exposing (root)

import Html exposing (Html, div, text)
import WindowSize.Types exposing (Model, Msg)


root : Model -> Html Msg
root model =
    div []
        [ div []
            [ text ("Width: " ++ toString model.windowSize.width)
            ]
        , div []
            [ text ("Height: " ++ toString model.windowSize.height)
            ]
        ]
