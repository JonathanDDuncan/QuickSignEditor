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


displayChoosings :
    MainChooser.Types.Model
    -> List (Html MainChooser.Types.Msg)
displayChoosings model =
    List.map displayChoosing model.choosings


displayhandChoosings :
    List Choosing.Types.Model
    -> List (Html MainChooser.Types.Msg)
displayhandChoosings model =
    List.map displayChoosing model


displayChoosing : Choosing.Types.Model -> Html MainChooser.Types.Msg
displayChoosing choosing =
    div [ onClick (Clicked choosing.value) ] [ App.map Choosing (Choosing.View.root choosing) ]


displayhandChoosing : Choosing.Types.Model -> Html MainChooser.Types.Msg
displayhandChoosing choosing =
    div [ onClick (Clicked choosing.value) ] [ App.map Choosing (Choosing.View.root choosing) ]


root : MainChooser.Types.Model -> Html MainChooser.Types.Msg
root model =
    div []
        [ div []
            (displayChoosings model)
        , choosesubgroupchooser model
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
    div [ style [ "position" => "relative" ] ]
        [ text "handgroupchooser"
        , fistrowchooser model
        , circlerowchooser model
        , cuprowchooser model
        , anglerowchooser model
        , flatrowchooser model
        ]


fistrowchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
fistrowchooser model =
    div [ style [ "height" => "100%", "clear" => "both" ] ]
        [ handcolumn model.handgroupchoosings.fistbabycommon
        , handcolumn model.handgroupchoosings.fistringcommon
        , handcolumn model.handgroupchoosings.fistmiddlecommon
        , handcolumn model.handgroupchoosings.fistindexcommon
        , handcolumn model.handgroupchoosings.fistthumbcommon
        ]


circlerowchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
circlerowchooser model =
    div [ style [ "height" => "100%", "clear" => "both" ] ]
        [ handcolumn model.handgroupchoosings.circlethumbcommon
        , handcolumn model.handgroupchoosings.circleindexcommon
        , handcolumn model.handgroupchoosings.circleringcommon
        , handcolumn model.handgroupchoosings.circlebabycommon
        ]


cuprowchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
cuprowchooser model =
    div [ style [ "height" => "100%", "clear" => "both" ] ]
        [ handcolumn model.handgroupchoosings.cupbabycommon
        , handcolumn model.handgroupchoosings.cupthumbcommon
        , handcolumn model.handgroupchoosings.cupindexcommon
        ]


anglerowchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
anglerowchooser model =
    div [ style [ "height" => "100%", "clear" => "both" ] ]
        [ handcolumn model.handgroupchoosings.anglethumbcommon
        , handcolumn model.handgroupchoosings.anglebabycommon
        ]


flatrowchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
flatrowchooser model =
    div [ style [ "height" => "100%", "clear" => "both" ] ]
        [ handcolumn model.handgroupchoosings.flatthumbcommon
        , handcolumn model.handgroupchoosings.flatbabycommon
        ]


handcolumn : List Choosing.Types.Model -> Html MainChooser.Types.Msg
handcolumn model =
    div []
        [ div []
            (displayhandChoosings model)
        ]


nogroupchooser : a -> Html b
nogroupchooser model =
    div []
        [ text "nogroupchooser" ]
