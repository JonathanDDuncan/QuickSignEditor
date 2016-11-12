module MainChooser.GeneralGroupChooserView exposing (generalgroupchooser)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)
import Exts.List exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SWEditor.Display exposing (signView, symbolaloneView)
import Material.Tooltip as Tooltip exposing (..)
import Material.Options as Options exposing (div, cs, when)


generalgroupchooser model choosings =
    let
        maxheight =
            3

        rowvalues =
            List.sort <| unique <| List.map (\item -> item.subgroup1) choosings
    in
        table []
            (List.map (\row -> rowchooser model row choosings maxheight) rowvalues)


rowchooser model row choosings maxheight =
    let
        items =
            List.filter (\item -> item.subgroup1 == row) choosings

        colvalues =
            List.sort <| unique <| List.map (\item -> item.plane) choosings
    in
        tr
            []
            (List.map (\col -> column model row col maxheight items) colvalues)


column model cat col choosingshigh choosings =
    let
        choosingsforcolumn =
            List.filter (\item -> item.plane == col) choosings
    in
        td
            [ class "chosercolumn"
            , style
                [ "background-color" => (bkcolor cat col) ]
            ]
            (choosingsforcolumn |> List.map (displaySymbolChoosing model))


displaySymbolChoosing model chooseritem =
    let
        symbol =
            getSymbolEditorBaseFillRotation chooseritem.base 1 1 model.symbolsizes

        mdlid =
            symbol.code + 1000
    in
        Html.div
            [ onClick (GroupSelected chooseritem)
            ]
            [ Options.div
                [ Tooltip.attach Mdl [ mdlid ] ]
                [ App.map SignView
                    (symbolaloneView symbol 3)
                ]
            , Tooltip.render Mdl
                [ mdlid ]
                model.mdl
                [ Tooltip.left ]
                [ Html.div [ attribute "style" "float:left;" ] [ text chooseritem.name ]
                ]
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
