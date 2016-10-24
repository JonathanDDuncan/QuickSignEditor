module MainChooser.View exposing (root)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)
import Choosing.View exposing (..)
import Choosing.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)
import Exts.Html exposing (..)
import Exts.List exposing (..)
import SWEditor.EditorSymbol exposing (..)
import String exposing (..)


--import SubMainChooser.View exposing (root)


displayChoosing : Choosing.Types.Model -> Html MainChooser.Types.Msg
displayChoosing choosing =
    div [ onClick (Clicked choosing.value) ] [ App.map Choosing (Choosing.View.root choosing) ]


displayhandChoosing : Choosing.Types.Model -> Html MainChooser.Types.Msg
displayhandChoosing choosing =
    let
        key =
            (Maybe.withDefault SWEditor.EditorSymbol.symbolinit (List.head choosing.displaySign.syms)).key
    in
        div [ onClick (Clicked choosing.value), class "choosing", style [ "height" => px (choosing.displaySign.height + 1) ] ]
            [ a [ class "tooltip", href "#" ]
                [ App.map Choosing (Choosing.View.normal choosing)
                , span []
                    [ span [ class (handpngcss key), attribute "style" "float:left;" ]
                        []
                    ]
                ]
            ]


handpngcss : String -> String
handpngcss key =
    String.toLower "hands-" ++ String.slice 1 4 key ++ "10"


root : MainChooser.Types.Model -> Html MainChooser.Types.Msg
root model =
    div []
        [ div []
            (List.map displayChoosing model.choosings)
        , div
            [ style [ "width" => "50%", "height" => "150px", "margin-left" => "50%", "margin-top" => "5px" ] ]
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
    table []
        [ fistrowchooser model 4
        , circlerowchooser model 4
        , cuprowchooser model 4
        , anglerowchooser model 4
        , flatrowchooser model 4
        ]


fistrowchooser : MainChooser.Types.Model -> Int -> Html MainChooser.Types.Msg
fistrowchooser model choosingshigh =
    tr
        [ style
            [ "height" => "50px", "clear" => "left" ]
        ]
        [ column 1 1 5 model.handgroupchoosings.fistthumbcommon
        , column 1 2 choosingshigh model.handgroupchoosings.fistindexcommon
        , column 1 3 choosingshigh model.handgroupchoosings.fistmiddlecommon
        , column 1 4 choosingshigh model.handgroupchoosings.fistringcommon
        , column 1 5 choosingshigh model.handgroupchoosings.fistbabycommon
        ]


bkcolor cat col =
    case cat of
        1 ->
            case col of
                1 ->
                    "#FF1111"

                2 ->
                    "#FF0000"

                3 ->
                    "#D30000"

                4 ->
                    "#A80000"

                _ ->
                    "#7C0000"

        2 ->
            case col of
                1 ->
                    "#FF7D11"

                2 ->
                    "#FF7400"

                3 ->
                    "#D36000"

                4 ->
                    "#A84C00"

                _ ->
                    "#7C3800"

        3 ->
            case col of
                1 ->
                    "#FFD611"

                2 ->
                    "#FFD300"

                3 ->
                    "#D3AF00"

                4 ->
                    "#A88C00"

                _ ->
                    "#7C6700"

        4 ->
            case col of
                1 ->
                    "#0FDD0F"

                2 ->
                    "#00CE00"

                3 ->
                    "#00A900"

                4 ->
                    "#008700"

                _ ->
                    "#006300"

        _ ->
            case col of
                1 ->
                    "#2150C6"

                2 ->
                    "#0B39AF"

                3 ->
                    "#072D8E"

                4 ->
                    "#042371"

                _ ->
                    "#021953"


column : Int -> Int -> Int -> List Choosing.Types.Model -> Html MainChooser.Types.Msg
column cat col choosingshigh choosings =
    td
        [ class "chosercolumn"
        , style
            [ "background-color" => (bkcolor cat col) ]
        ]
        [ span
            []
            (List.map
                (handcolumn)
                (nomorethan choosingshigh choosings)
            )
        ]


circlerowchooser : MainChooser.Types.Model -> Int -> Html MainChooser.Types.Msg
circlerowchooser model choosingshigh =
    tr
        [ style
            [ "height" => "50px", "clear" => "left" ]
        ]
        [ column 2 1 choosingshigh model.handgroupchoosings.circlethumbcommon
        , column 2 2 choosingshigh model.handgroupchoosings.circleindexcommon
        , spacercolumn
        , column 2 4 choosingshigh model.handgroupchoosings.circleringcommon
        , column 2 5 choosingshigh model.handgroupchoosings.circlebabycommon
        ]


cuprowchooser : MainChooser.Types.Model -> Int -> Html MainChooser.Types.Msg
cuprowchooser model choosingshigh =
    tr
        [ style
            [ "height" => "50px", "clear" => "left" ]
        ]
        [ column 3 1 choosingshigh model.handgroupchoosings.cupthumbcommon
        , column 3 2 choosingshigh model.handgroupchoosings.cupindexcommon
        , spacercolumn
        , spacercolumn
        , column 3 5 choosingshigh model.handgroupchoosings.cupbabycommon
        ]


anglerowchooser : MainChooser.Types.Model -> Int -> Html MainChooser.Types.Msg
anglerowchooser model choosingshigh =
    tr
        [ style
            [ "height" => "50px", "clear" => "left" ]
        ]
        [ column 4 1 choosingshigh model.handgroupchoosings.anglethumbcommon
        , spacercolumn
        , spacercolumn
        , spacercolumn
        , column 4 5 choosingshigh model.handgroupchoosings.anglebabycommon
        ]


flatrowchooser : MainChooser.Types.Model -> Int -> Html MainChooser.Types.Msg
flatrowchooser model choosingshigh =
    tr
        [ style
            [ "height" => "50px", "clear" => "left" ]
        ]
        [ column 5 1 choosingshigh model.handgroupchoosings.flatthumbcommon
        , spacercolumn
        , spacercolumn
        , spacercolumn
        , column 5 5 choosingshigh model.handgroupchoosings.flatbabycommon
        ]


handcolumn : List Choosing.Types.Model -> Html MainChooser.Types.Msg
handcolumn choosings =
    span
        [ style
            [ "width" => "23px", "float" => "left", "margin-top" => "5px" ]
        ]
        (List.map (displayhandChoosing) choosings)


nomorethan : Int -> List a -> List (List a)
nomorethan num choosings =
    chunk num choosings


spacercolumn : Html MainChooser.Types.Msg
spacercolumn =
    td
        []
        [ text nbsp ]



-- "margin-left" => (toString (20 * (col - 1)) ++ "%"),


nogroupchooser : a -> Html b
nogroupchooser model =
    div []
        [ text "nogroupchooser" ]
