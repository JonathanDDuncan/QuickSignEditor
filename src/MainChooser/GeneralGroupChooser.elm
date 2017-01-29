module MainChooser.GeneralGroupChooser exposing (generalgroupchooser, creategeneralgroupchooserdata)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.Display exposing (signView, symbolaloneView)
import Material.Tooltip as Tooltip exposing (attach, render, left)
import Material.Options as Options exposing (div, cs, when)
import SWEditor.EditorSymbol exposing (..)
import Exts.List exposing (..)


-- View


generalgroupchooser : List (List GeneralGroupChooserColumData) -> Html Msg
generalgroupchooser tabledata =
    table []
        (List.map row tabledata)


row : List GeneralGroupChooserColumData -> Html Msg
row rowdata =
    tr
        []
        (List.map column rowdata)


column : GeneralGroupChooserColumData -> Html Msg
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


symbol : GeneralGroupChooserSymbolData -> Html Msg
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



--State functions


creategeneralgroupchooserdata : Model -> String -> List (List GeneralGroupChooserColumData)
creategeneralgroupchooserdata model basesymbol =
    let
        choosings =
            getchoosings basesymbol model.allgroupchoosings

        rowvalues =
            List.sort <| unique <| List.map (\item -> item.row) choosings

        tabledata =
            creategeneralgroupchoosertabledata model choosings rowvalues
    in
        tabledata


creategeneralgroupchoosertabledata : MainChooser.Types.Model -> List ChooserItem -> List Int -> List (List GeneralGroupChooserColumData)
creategeneralgroupchoosertabledata model choosings rowvalues =
    List.map
        (\row ->
            creategeneralgroupchooserrowdata model row choosings
        )
        rowvalues


creategeneralgroupchooserrowdata : MainChooser.Types.Model -> Int -> List ChooserItem -> List GeneralGroupChooserColumData
creategeneralgroupchooserrowdata model row choosings =
    let
        colvalues =
            List.sort <| unique <| List.map (\item -> item.col) choosings

        rowdata =
            (List.map (\col -> creategeneralgroupchoosercolumndata model row col choosings) colvalues)
    in
        rowdata


creategeneralgroupchoosercolumndata : MainChooser.Types.Model -> Int -> Int -> List ChooserItem -> GeneralGroupChooserColumData
creategeneralgroupchoosercolumndata model row col choosings =
    let
        choosingsforcolumn =
            List.filter (\item -> item.col == col) choosings

        symboldatalist =
            (choosingsforcolumn
                |> List.map
                    (\chooseritem ->
                        creategeneralgroupchoosersymboldata model chooseritem
                    )
            )
    in
        { symboldatalist = symboldatalist, row = row, col = col }


creategeneralgroupchoosersymboldata : MainChooser.Types.Model -> ChooserItem -> GeneralGroupChooserSymbolData
creategeneralgroupchoosersymboldata model chooseritem =
    let
        symbol =
            getSymbolEditorBaseFillRotation chooseritem.base 1 1 model.symbolsizes

        mdlid =
            symbol.code + 1000

        modelmdl =
            model.mdl
    in
        { modelmdl = modelmdl, chooseritem = chooseritem, symbol = symbol, mdlid = mdlid }
