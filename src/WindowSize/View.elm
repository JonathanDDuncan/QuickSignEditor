module WindowSize.View exposing (root)

import Html exposing (..)
import WindowSize.Types exposing (..)


--import SubWindow.View exposing (root)


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