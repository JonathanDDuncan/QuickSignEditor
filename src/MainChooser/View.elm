module MainChooser.View exposing (root)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)
import Choosing.View exposing (..)
import Choosing.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)


--import SubMainChooser.View exposing (root)


displayChoosing : Choosing.Types.Model -> Html MainChooser.Types.Msg
displayChoosing choosing =
    div [ onClick (Clicked choosing.value) ] [ App.map Choosing (Choosing.View.root choosing) ]


displayhandChoosing : Int -> Choosing.Types.Model -> Html MainChooser.Types.Msg
displayhandChoosing col choosing =
    div [ onClick (Clicked choosing.value), class "choosing", style [ "height" => "20px" ] ] [ App.map Choosing (Choosing.View.normal choosing) ]


root : MainChooser.Types.Model -> Html MainChooser.Types.Msg
root model =
    div []
        [ div []
            (List.map displayChoosing model.choosings)
        , div
            [ style [ "background-color" => "blue", "width" => "50%", "height" => "150px", "margin-left" => "50%" ] ]
            [ choosesubgroupchooser model
            ]
        ]


choosesubgroupchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
choosesubgroupchooser model =
    case model.clicked of
        "S14c10" ->
            handgroupchooser model

        "S14c18" ->
            handgroupchooser model

        _ ->
            nogroupchooser model


handgroupchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
handgroupchooser model =
    div []
        [ fistrowchooser model
          -- , circlerowchooser model
          -- , cuprowchooser model
          -- , anglerowchooser model
          -- , flatrowchooser model
        ]


fistrowchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
fistrowchooser model =
    div []
        [ handcolumn model.handgroupchoosings.fistthumbcommon 1
          -- , handcolumn model.handgroupchoosings.fistindexcommon 2
          -- , handcolumn model.handgroupchoosings.fistmiddlecommon 3
          -- , handcolumn model.handgroupchoosings.fistringcommon 4
          -- , handcolumn model.handgroupchoosings.fistbabycommon 5
        ]


circlerowchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
circlerowchooser model =
    div [ class "fifths" ]
        [ handcolumn model.handgroupchoosings.circlethumbcommon 1
        , handcolumn model.handgroupchoosings.circleindexcommon 2
        , handcolumn model.handgroupchoosings.circleringcommon 4
        , handcolumn model.handgroupchoosings.circlebabycommon 5
        ]


cuprowchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
cuprowchooser model =
    div [ class "fifths" ]
        [ handcolumn model.handgroupchoosings.cupthumbcommon 1
        , handcolumn model.handgroupchoosings.cupindexcommon 2
        , handcolumn model.handgroupchoosings.cupbabycommon 5
        ]


anglerowchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
anglerowchooser model =
    div [ class "fifths" ]
        [ handcolumn model.handgroupchoosings.anglethumbcommon 0
        , handcolumn model.handgroupchoosings.anglebabycommon 5
        ]


flatrowchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
flatrowchooser model =
    div [ class "fifths" ]
        [ handcolumn model.handgroupchoosings.flatthumbcommon 0
        , handcolumn model.handgroupchoosings.flatbabycommon 5
        ]


handcolumn : List Choosing.Types.Model -> Int -> Html MainChooser.Types.Msg
handcolumn choosings col =
    div
        [ style
            [ "width" => "20%", "margin-left" => (toString (20 * (col - 1)) ++ "%") ]
        ]
        (List.map (displayhandChoosing col) choosings)


nogroupchooser : a -> Html b
nogroupchooser model =
    div []
        [ text "nogroupchooser" ]
