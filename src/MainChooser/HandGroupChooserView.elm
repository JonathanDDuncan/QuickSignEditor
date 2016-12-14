module MainChooser.HandGroupChooserView exposing (handgroupchooser)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)
import Exts.List exposing (..)
import String exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SWEditor.Display exposing (signView, symbolaloneView)
import Material.Tooltip as Tooltip exposing (..)
import Material.Options as Options exposing (div, cs, when)


handgroupchooser : MainChooser.Types.Model -> List ChooserItem -> Html MainChooser.Types.Msg
handgroupchooser model handgroupchoosings =
    let
        maxheight =
            3

        rowvalues =
            List.sort <| unique <| List.map (\item -> item.row) handgroupchoosings
    in
        Html.div []
            [ Html.div []
                [ button [ Html.Events.onClick (FilterHandGroup 1) ] [ text "common" ]
                , button [ Html.Events.onClick (FilterHandGroup 2) ] [ text "not common" ]
                , button [ Html.Events.onClick (FilterHandGroup 3) ] [ text "all" ]
                ]
            , table []
                (List.concatMap (\row -> rowchooser model row handgroupchoosings maxheight) rowvalues)
            ]


rowchooser : MainChooser.Types.Model -> Int -> List ChooserItem -> Int -> List (Html MainChooser.Types.Msg)
rowchooser model row handgroupchoosings maxheight =
    let
        items =
            List.filter (\item -> item.row == Debug.log "row" row) handgroupchoosings

        withoutthumbs =
            List.filter (\item -> not item.thumb) items

        withthumbs =
            List.filter (\item -> item.thumb) items

        colvalues =
            [1..5]
    in
        [ if (Debug.log "withoutthumbs" <| List.length withoutthumbs ) > 0 then
            tr
                []
                (List.map (\col -> column model row col maxheight withoutthumbs) colvalues)
          else
            text ""
        , if (Debug.log "withthumbs" <| List.length withthumbs) > 0 then
            tr
                []
                (List.map (\col -> column model row col maxheight withthumbs) colvalues)
          else
            text ""
        ]


column model cat col choosingshigh choosings =
    let
        handgroupfilter =
            model.handgroupfilter

        items =
            case handgroupfilter of
                2 ->
                    List.filter (\item -> item.col == col && item.common == False) choosings

                3 ->
                    List.filter (\item -> item.col == col) choosings

                _ ->
                    List.filter (\item -> item.col == col && item.common == True) choosings
    in
        td
            [ class "chosercolumn"
            , style
                [ "background-color" => (bkcolor cat col) ]
            ]
            (List.map (displayhandChoosing model) items)


nomorethan : Int -> List a -> List (List a)
nomorethan num choosings =
    chunk num choosings


displayhandChoosing model chooseritem =
    let
        base =
            chooseritem.base

        fill =
            2

        rotation =
            0

        symbol =
            getSymbolEditorBaseFillRotation base fill rotation model.symbolsizes

        mdlid =
            symbol.code + 1000
    in
        Html.div
            [ Html.Events.onClick (GroupSelected chooseritem)
            ]
            [ Options.div
                [ Tooltip.attach Mdl [ mdlid ] ]
                [ App.map SignView
                    (symbolaloneView symbol 5)
                ]
            , Tooltip.render Mdl
                [ mdlid ]
                model.mdl
                [ Tooltip.left ]
                [ span [ class (handpngcss chooseritem.symbolkey), attribute "style" "display:inline-block ;margin: auto;" ] []
                , Html.div [ attribute "style" "width:100%;" ] [ text chooseritem.name ]
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
