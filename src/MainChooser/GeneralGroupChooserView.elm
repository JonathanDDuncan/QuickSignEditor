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
import SWEditor.Display exposing (signView, symbolaloneView)
import Dict exposing (..)
import SW.Types exposing (..)


generalgroupchooser : Dict String Size -> List ChooserItem -> Html MainChooser.Types.Msg
generalgroupchooser symbolsizes choosings =
    let
        maxheight =
            3

        rowvalues =
            List.sort <| unique <| List.map (\item -> item.subgroup1) choosings
    in
        table []
            (List.map (\row -> rowchooser row choosings maxheight symbolsizes) rowvalues)


rowchooser row choosings maxheight symbolsizes =
    let
        items =
            List.filter (\item -> item.subgroup1 == row) choosings

        colvalues =
            List.sort <| unique <| List.map (\item -> item.plane) choosings
    in
        tr
            []
            (List.map (\col -> column row col maxheight items symbolsizes) colvalues)


column : Int -> Int -> Int -> List ChooserItem -> Dict String Size -> Html MainChooser.Types.Msg
column cat col choosingshigh choosings symbolsizes =
    let
        choosingsforcolumn =
            List.filter (\item -> item.plane == col) choosings
    in
        td
            [ class "chosercolumn"
            , style
                [ "background-color" => (bkcolor cat col) ]
            ]
            (choosingsforcolumn |> List.map (displayhandChoosing symbolsizes))


spacercolumn : Html MainChooser.Types.Msg
spacercolumn =
    td
        []
        [ text nbsp ]


displayhandChoosing : Dict String Size -> ChooserItem -> Html MainChooser.Types.Msg
displayhandChoosing symbolsizes chooseritem =
    let
        symbol =
            getSymbolEditorBaseFillRotation chooseritem.base 1 1 symbolsizes
    in
        div
            [ onClick (GroupSelected chooseritem)
            ]
            [ App.map SignView
                (symbolaloneView symbol 3)
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
