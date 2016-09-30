module MainChooser.View exposing (root)

import Html exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)


--import SubMainChooser.View exposing (root)


root : Model -> Html Msg
root model =
    div []
        [ text "Put the view here"
        , button [ onClick RequestInitialChoosings ] [ text "Setup Chooser" ]
        ]
