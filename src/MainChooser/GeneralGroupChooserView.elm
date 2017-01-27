module MainChooser.GeneralGroupChooserView exposing (generalgroupchooser)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)
import Exts.List exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SWEditor.Display exposing (signView, symbolaloneView)
import Material.Tooltip as Tooltip exposing (attach, render, left)
import Material.Options as Options exposing (div, cs, when)


generalgroupchooser : Model -> List ChooserItem -> Html MainChooser.Types.Msg
generalgroupchooser model choosings =
    let
        maxheight =
            3

        rowvalues =
            List.sort <| unique <| List.map (\item -> item.row) choosings
    in
        table []
            (List.map (\row -> rowchooser model row choosings maxheight) rowvalues)


rowchooser : Model -> Int -> List ChooserItem -> b -> Html Msg
rowchooser model row choosings maxheight =
    let
        items =
            List.filter (\item -> item.row == row) choosings

        colvalues =
            List.sort <| unique <| List.map (\item -> item.col) choosings
    in
        tr
            []
            (List.map (\col -> column model row col maxheight items) colvalues)


column : Model -> Int -> Int -> a -> List ChooserItem -> Html Msg
column model cat col choosingshigh choosings =
    let
        choosingsforcolumn =
            List.filter (\item -> item.col == col) choosings
    in
        td
            [ class "chosercolumn"
            , style
                [ "background-color" => (bkcolor cat col) ]
            ]
            (choosingsforcolumn |> List.map (displaySymbolChoosing model))


displaySymbolChoosing : Model -> ChooserItem -> Html Msg
displaySymbolChoosing model chooseritem =
    let
        symbol =
            getSymbolEditorBaseFillRotation chooseritem.base 1 1 model.symbolsizes

        mdlid =
            symbol.code + 1000
    in
        Html.div
            [ onClick (GroupSelected chooseritem)
            , onMouseDown (DragSymbol symbol.code)
            , onDoubleClick (ReplaceSymbol symbol.code)
            ]
            [ Options.div
                [ Tooltip.attach Mdl [ mdlid ] ]
                [ Html.map SignView
                    (symbolaloneView symbol 3)
                ]
            , Tooltip.render Mdl
                [ mdlid ]
                model.mdl
                [ Tooltip.left ]
                [ Html.div [ attribute "style" "float:left;" ] [ text chooseritem.name ]
                ]
            ]
