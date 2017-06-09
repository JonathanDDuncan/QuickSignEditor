module Choosers.GeneralGroupChooser exposing (generalgroupchooser, creategeneralgroupchooserdata)

import Html exposing (Html, table, tr, td)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onMouseDown, onDoubleClick)
import Choosers.Types
    exposing
        ( Model
        , Msg(EditorMsg)
        , Editor(GroupSelected, DragSymbol, ReplaceSymbol)
        , ChooserItem
        , GeneralGroupChooserColumData
        , GeneralGroupChooserSymbolData
        , bkcolor
        , getchoosings
        )
import Helpers.ViewExtra exposing (px, (=>))
import SW.Display exposing (symbolsvg)
import SWEditor.EditorSymbol exposing (getSymbolbyBaseFillRotation)
import Exts.List exposing (unique)
import SW.Pua exposing (codefromkey)
import Choosers.SymbolToolTip exposing (symboltooltip)


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
        (List.map symbol columndata.symboldatalist)


symbol : GeneralGroupChooserSymbolData -> Html Msg
symbol symboldata =
    Html.div
        [ onClick ((EditorMsg << GroupSelected) symboldata.chooseritem)
        , onMouseDown ((EditorMsg << DragSymbol) symboldata.symbol.key)
        , onDoubleClick ((EditorMsg << ReplaceSymbol) symboldata.symbol.key)
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


creategeneralgroupchooserdata : Model -> List (List GeneralGroupChooserColumData)
creategeneralgroupchooserdata model =
    let
        choosings =
            model.clicked
                |> String.slice 0 4
                |> getchoosings model.groupchoosings

        rowvalues =
            choosings
                |> List.map (\item -> item.row)
                |> unique
    in
        creategeneralgroupchoosertabledata model choosings rowvalues


creategeneralgroupchoosertabledata :
    Choosers.Types.Model
    -> List ChooserItem
    -> List Int
    -> List (List GeneralGroupChooserColumData)
creategeneralgroupchoosertabledata model choosings rowvalues =
    List.map
        (\row ->
            creategeneralgroupchooserrowdata model row choosings
        )
        rowvalues


creategeneralgroupchooserrowdata :
    Choosers.Types.Model
    -> Int
    -> List ChooserItem
    -> List GeneralGroupChooserColumData
creategeneralgroupchooserrowdata model row choosings =
    choosings
        |> List.map (\item -> item.col)
        |> List.sort
        |> List.map (\col -> creategeneralgroupchoosercolumndata model row col choosings)


creategeneralgroupchoosercolumndata :
    Choosers.Types.Model
    -> Int
    -> Int
    -> List ChooserItem
    -> GeneralGroupChooserColumData
creategeneralgroupchoosercolumndata model row col choosings =
    { symboldatalist =
        choosings
            |> List.filter (\item -> item.col == col && item.row == row)
            |> List.map
                (\chooseritem ->
                    creategeneralgroupchoosersymboldata model chooseritem
                )
    , row = row
    , col = col
    }


creategeneralgroupchoosersymboldata : Choosers.Types.Model -> ChooserItem -> GeneralGroupChooserSymbolData
creategeneralgroupchoosersymboldata model chooseritem =
    let
        symbol =
            getSymbolbyBaseFillRotation chooseritem.base 1 1 model.symbolsizes
    in
        { modelmdl = model.mdl
        , chooseritem = chooseritem
        , symbol = symbol
        , mdlid = codefromkey symbol.key + 1000
        }
