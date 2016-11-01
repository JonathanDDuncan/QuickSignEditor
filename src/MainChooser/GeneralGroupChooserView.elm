module MainChooser.GeneralGroupChooserView exposing (generalgroupchooser)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)
import Exts.Html exposing (..)
import Exts.List exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SWEditor.Display exposing (signView)


generalgroupchooser : List ChooserItem -> Html MainChooser.Types.Msg
generalgroupchooser choosings =
    let
        maxheight =
            3

        rowvalues =
            List.sort <| unique <| List.map (\item -> item.subgroup1) choosings
    in
        table []
            (List.map (\row -> rowchooser row choosings maxheight) rowvalues)


rowchooser row choosings maxheight =
    let
        items =
            List.filter (\item -> item.subgroup1 == row) choosings

        colvalues =
            List.sort <| unique <| List.map (\item ->   item.plane) choosings
    in
        tr
            []
            (List.map (\col -> column row col maxheight items) colvalues)


column : Int -> Int -> Int -> List ChooserItem -> Html MainChooser.Types.Msg
column cat col choosingshigh choosings =
 let
   choosingsforcolumn = List.filter (\item -> item.plane == col )choosings
 in
   
    td
        [ class "chosercolumn"
        , style
            [ "background-color" => (bkcolor cat col) ]
        ]
        [ span
            []
            [ handcolumn
                choosingsforcolumn
            ]
        ]


handcolumn : List ChooserItem -> Html MainChooser.Types.Msg
handcolumn choosings =
    span
        [ style
            [ "width" => "23px", "float" => "left", "margin-top" => "5px" ]
        ]
        (List.map displayhandChoosing choosings)


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
            1

        rotation =
            1

        symbol =
            getSymbolEditor base fill rotation

        sign =
            { syms = [ symbol ] }
    in
        div
            [ onClick (GroupSelected chooseritem.base)
            , class "choosing"
            ]
            [ App.map SignView
                (signView sign
                    [ Html.Attributes.style
                        [ "position" => "relative"
                        , "transform" => "scale(1)"
                        , "margin" => "2px"
                        , "height" => "100%"
                        ]
                    ]
                )
            ]


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
