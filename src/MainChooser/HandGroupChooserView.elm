module MainChooser.HandGroupChooserView exposing (handgroupchooser)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)
import Exts.Html exposing (..)
import Exts.List exposing (..)
import String exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SWEditor.Display exposing (signView)


handgroupchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
handgroupchooser model =
    let
        maxheight =
            3

        rowvalues =
            List.sort <| unique <| List.map (\item -> item.subgroup1) model.handgroupchoosings
    in
        table []
            (List.map (\row -> rowchooser row model.handgroupchoosings maxheight) rowvalues)


rowchooser row handgroupchoosings maxheight =
    let
        items =
            List.filter (\item -> item.subgroup1 == row) handgroupchoosings

        colvalues =
            List.sort <| unique <| List.map (\item -> item.subgroup2) handgroupchoosings
    in
        tr
            []
            (List.map (\col -> column row col maxheight items) colvalues)


column : Int -> Int -> Int -> HandGroupModel -> Html MainChooser.Types.Msg
column cat col choosingshigh choosings =
    let
        items =
            List.filter (\item -> item.subgroup2 == col && item.common == True) choosings
    in
        td
            [ class "chosercolumn"
            , style
                [ "background-color" => (bkcolor cat col) ]
            ]
            [ span
                []
                (List.map
                    (handcolumn)
                    (nomorethan choosingshigh items)
                )
            ]


handcolumn : HandGroupModel -> Html MainChooser.Types.Msg
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


displayhandChoosing : ChooserItem -> Html MainChooser.Types.Msg
displayhandChoosing chooseritem =
    let
        base =
            chooseritem.base

        fill =
            2

        rotation =
            0

        symbol =
            getSymbolEditor base fill rotation

        sign =
            { syms = [ symbol ] }
    in
        div [ onClick (GroupSelected chooseritem.base), class "choosing", style [ "height" => px 20 ] ]
            [ a [ class "tooltip", href "#" ]
                [ App.map SignView
                    (signView sign
                        [ Html.Attributes.style
                            [ "position" => "relative"
                            , "left" => px 0
                            , "top" => px 0
                            , "width" => px 44
                            , "height" => px 44
                            , "margin" => "4px"
                            ]
                        ]
                    )
                , span []
                    [ span [ class (handpngcss chooseritem.symbolkey), attribute "style" "float:left;" ]
                        []
                    ]
                ]
            ]


handpngcss : String -> String
handpngcss key =
    String.toLower "hands-" ++ String.slice 1 4 key ++ "10"


bkcolor : number -> number' -> String
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
