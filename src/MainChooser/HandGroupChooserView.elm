module MainChooser.HandGroupChooserView exposing (handgroupchooser)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.Display exposing (signView, symbolaloneView)
import SWEditor.SymbolToolTip exposing (..)
import Material
import SWEditor.EditorSymbol exposing (EditorSymbol)


handgroupchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
handgroupchooser model =
    let
        tabledata =
            createtabledata model
    in
        Html.div []
            [ Html.div []
                [ button [ Html.Events.onClick (FilterHandGroup 1) ] [ text "common" ]
                , button [ Html.Events.onClick (FilterHandGroup 2) ] [ text "not common" ]
                , button [ Html.Events.onClick (FilterHandGroup 3) ] [ text "all" ]
                ]
            , table []
                (List.concatMap rowchooser tabledata)
            ]


createtabledata : Model -> List (List (List ColumnData))
createtabledata model =
    (List.map
        (\data ->
            let
                tabledata2 =
                    createrowdata model data
            in
                tabledata2
        )
        model.handgroupchooseritems
    )


rowchooser : List (List ColumnData) -> List (Html Msg)
rowchooser tabledata =
    List.map row tabledata


createrowdata : Model -> List (List HandGroupChooserSubList) -> List (List ColumnData)
createrowdata model tabledata =
    let
        filtered =
            List.filter
                (\columndata ->
                    List.length columndata > 0
                )
                tabledata

        rowdata =
            List.map
                (\rowdata1 ->
                    createcolumndata model rowdata1
                )
                filtered
    in
        rowdata


row : List ColumnData -> Html Msg
row columndata =
    tr
        []
        (List.map column columndata)


createcolumndata : Model -> List HandGroupChooserSubList -> List ColumnData
createcolumndata model rowdata =
    (List.map
        (\coldata ->
            let
                symboldatalist =
                    createsymboldatalist model coldata
            in
                { symboldatalist = symboldatalist, backgroundcolor = coldata.backgroundcolor }
        )
        rowdata
    )


type alias ColumnData =
    { backgroundcolor : String
    , symboldatalist : List SymbolData
    }


column : ColumnData -> Html Msg
column columndata =
    td
        [ class "chosercolumn"
        , style
            [ "background-color" => columndata.backgroundcolor ]
        ]
        (List.map symbol columndata.symboldatalist)


createsymboldatalist : Model -> HandGroupChooserSubList -> List SymbolData
createsymboldatalist model columndata =
    List.map
        (\displayhanditem ->
            { modelmdl = model.mdl, symbol = displayhanditem.symbol, chooseritem = displayhanditem.chooseritem, mdlid = displayhanditem.mdlid }
        )
        columndata.displayhanditems


symbol : SymbolData -> Html Msg
symbol symboldata =
    Html.div
        [ Html.Events.onClick (GroupSelected symboldata.chooseritem)
        , onMouseDown (DragSymbol symboldata.symbol.code)
        , onDoubleClick (ReplaceSymbol symboldata.symbol.code)
        ]
        (symboltooltip
            symboldata.modelmdl
            symboldata.mdlid
            symboldata.chooseritem.name
            symboldata.chooseritem.symbolkey
            1
            2
            RightThumbEdge
            [ Html.map SignView
                (symbolaloneView symboldata.symbol 5)
            ]
        )


type alias SymbolData =
    { chooseritem : ChooserItem
    , mdlid : Int
    , modelmdl : Material.Model
    , symbol :
        EditorSymbol
    }
