module MainChooser.View exposing (root)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)
import Choosing.View exposing (..)
import Choosing.Types exposing (..)


--import SubMainChooser.View exposing (root)


displayChoosings :
    List Choosing.Types.Model
    -> List (Html MainChooser.Types.Msg)
displayChoosings model =
    List.map displayChoosing model


displayChoosing : Choosing.Types.Model -> Html MainChooser.Types.Msg
displayChoosing choosing =
    App.map Choosing (Choosing.View.root choosing)


root : MainChooser.Types.Model -> Html MainChooser.Types.Msg
root model =
    div []
        [ button [ onClick RequestInitialChoosings ] [ text "Setup Chooser" ]
        , div []
            (displayChoosings model)
        ]