module Choosers.GeneralGroupChooser exposing (generalgroupchooser, creategeneralgroupchooserdata)

import Html exposing (Html, table, tr, td)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onMouseDown, onDoubleClick)
import Choosers.Types exposing (Model, Msg(EditorMsg), bkcolor, getchoosings)
import Choosers.EditorType as Editor exposing (Editor)
import Choosers.ChooserItemType as ChooserItemType exposing (ChooserItem)
import Helpers.ViewExtra exposing (px, (=>))
import SW.Display exposing (symbolsvg)
import Exts.List exposing (unique)
import SW.Pua exposing (Fill, Base, Key, codefromkey)
import Choosers.SymbolToolTip exposing (symboltooltip)
import SW.Symbol exposing (Symbol, HandFills(..), Hands(..), Planes(..), createSymbolbyBaseFillRotation)
import Material as Material exposing (Model)


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
            [ "background-color" => bkcolor columndata.row ]
        ]
        (List.map
            symbol
            columndata.symboldatalist
        )


symbol : GeneralGroupChooserSymbolData -> Html Msg
symbol symboldata =
    Html.div
        [ onClick ((EditorMsg << Editor.GroupSelected) symboldata.chooseritem)
        , onMouseDown ((EditorMsg << Editor.DragSymbol) symboldata.symbol.key)
        , onDoubleClick ((EditorMsg << Editor.ReplaceSymbol) symboldata.symbol.key)
        , style
            [ "margin-top" => px 3 ]
        ]
        (symboltooltip
            symboldata.modelmdl
            symboldata.mdlid
            symboldata.chooseritem.name
            [ symbolsvg "hover" symboldata.symbol
            ]
        )



--State functions


creategeneralgroupchooserdata : Choosers.Types.Model -> List (List GeneralGroupChooserColumData)
creategeneralgroupchooserdata model =
    let
        basesymbol =
            String.slice 0 4 model.clicked

        choosings =
            getchoosings model.groupchoosings basesymbol

        rowvalues =
            List.sort <| unique <| List.map (\item -> item.row) choosings

        tabledata =
            creategeneralgroupchoosertabledata model choosings rowvalues
    in
        tabledata


creategeneralgroupchoosertabledata : Choosers.Types.Model -> List ChooserItem -> List Int -> List (List GeneralGroupChooserColumData)
creategeneralgroupchoosertabledata model choosings rowvalues =
    List.map
        (\row ->
            creategeneralgroupchooserrowdata model row choosings
        )
        rowvalues


creategeneralgroupchooserrowdata : Choosers.Types.Model -> Int -> List ChooserItem -> List GeneralGroupChooserColumData
creategeneralgroupchooserrowdata model row choosings =
    let
        colvalues =
            List.sort <| unique <| List.map (\item -> item.col) choosings

        rowdata =
            List.map (\col -> creategeneralgroupchoosercolumndata model row col choosings) colvalues
    in
        rowdata


creategeneralgroupchoosercolumndata : Choosers.Types.Model -> Int -> Int -> List ChooserItem -> GeneralGroupChooserColumData
creategeneralgroupchoosercolumndata model row col choosings =
    let
        choosingsforcolumn =
            List.filter (\item -> item.col == col && item.row == row) choosings

        symboldatalist =
            choosingsforcolumn
                |> List.map
                    (\chooseritem ->
                        creategeneralgroupchoosersymboldata model chooseritem
                    )
    in
        { symboldatalist = symboldatalist, row = row, col = col }


creategeneralgroupchoosersymboldata : Choosers.Types.Model -> ChooserItem -> GeneralGroupChooserSymbolData
creategeneralgroupchoosersymboldata model chooseritem =
    let
        symbol =
            createSymbolbyBaseFillRotation chooseritem.base 1 1 model.symbolsizes

        mdlid =
            codefromkey symbol.key + 1000

        modelmdl =
            model.mdl
    in
        { modelmdl = modelmdl, chooseritem = chooseritem, symbol = symbol, mdlid = mdlid }


type alias GeneralGroupChooserColumData =
    { col : Int
    , row : Int
    , symboldatalist : List GeneralGroupChooserSymbolData
    }


type alias GeneralGroupChooserSymbolData =
    { chooseritem : ChooserItem
    , mdlid : Int
    , modelmdl : Material.Model
    , symbol : Symbol
    }
