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
import Material


generalgroupchooser : Model -> List ChooserItem -> Html MainChooser.Types.Msg
generalgroupchooser model choosings =
    let
        rowvalues =
            List.sort <| unique <| List.map (\item -> item.row) choosings

        tabledata =
            createtabledata model choosings rowvalues
    in
        generalgroupchooser1 tabledata


generalgroupchooser1 tabledata =
    table []
        (List.map row tabledata)


createtabledata : Model -> List ChooserItem -> List Int -> List (List ColumData)
createtabledata model choosings rowvalues =
    List.map
        (\row ->
            createrowdata model row choosings
        )
        rowvalues


createrowdata : Model -> Int -> List ChooserItem -> List ColumData
createrowdata model row choosings =
    let
        colvalues =
            List.sort <| unique <| List.map (\item -> item.col) choosings

        rowdata =
            (List.map (\col -> coldata model row col choosings) colvalues)
    in
        rowdata


row rowdata =
    tr
        []
        (List.map column rowdata)


coldata :
    Model
    -> Int
    -> Int
    -> List ChooserItem
    -> ColumData
coldata model row col choosings =
    let
        choosingsforcolumn =
            List.filter (\item -> item.col == col) choosings

        symboldatalist =
            (choosingsforcolumn
                |> List.map
                    (\chooseritem ->
                        createsymboldata model chooseritem
                    )
            )
    in
        { symboldatalist = symboldatalist, row = row, col = col }


type alias ColumData =
    { col : Int, row : Int, symboldatalist : List SymbolData }


createsymboldata :
    Model
    -> ChooserItem
    -> SymbolData
createsymboldata model chooseritem =
    let
        symbol =
            getSymbolEditorBaseFillRotation chooseritem.base 1 1 model.symbolsizes

        mdlid =
            symbol.code + 1000

        modelmdl =
            model.mdl
    in
        { modelmdl = modelmdl, chooseritem = chooseritem, symbol = symbol, mdlid = mdlid }


type alias SymbolData =
    { chooseritem : ChooserItem
    , mdlid : Int
    , modelmdl : Material.Model
    , symbol : EditorSymbol
    }


column columndata =
    td
        [ class "chosercolumn"
        , style
            [ "background-color" => (bkcolor columndata.row columndata.col) ]
        ]
        (List.map
            symbol
            columndata.symboldatalist
        )


symbol symboldata =
    Html.div
        [ onClick (GroupSelected symboldata.chooseritem)
        , onMouseDown (DragSymbol symboldata.symbol.code)
        , onDoubleClick (ReplaceSymbol symboldata.symbol.code)
        ]
        [ Options.div
            [ Tooltip.attach Mdl [ symboldata.mdlid ] ]
            [ Html.map SignView
                (symbolaloneView symboldata.symbol 3)
            ]
        , Tooltip.render Mdl
            [ symboldata.mdlid ]
            symboldata.modelmdl
            [ Tooltip.left ]
            [ Html.div [ attribute "style" "float:left;" ] [ text symboldata.chooseritem.name ]
            ]
        ]
