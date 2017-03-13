module Choosers.HandGroupChooser exposing (gethandgroupchooserdata, createhandgroupchooserdata, handgroupchooser)

import Choosers.Types exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SW.Types exposing (..)
import Dict exposing (..)
import String exposing (..)
import List.Extra exposing (..)
import SW.Pua exposing (codefromkey)


--View

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Choosers.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.DisplaySvg exposing (symbolsvg)
import SWEditor.SymbolToolTip exposing (..)


handgroupchooser : List (List HandGroupChooserViewColumnData) -> Html Msg
handgroupchooser tabledata =
    Html.div []
        [ Html.div []
            [ button [ Html.Events.onClick (FilterHandGroup 1) ] [ text "common" ]
            , button [ Html.Events.onClick (FilterHandGroup 2) ] [ text "not common" ]
            , button [ Html.Events.onClick (FilterHandGroup 3) ] [ text "all" ]
            ]
        , table []
            (List.map row tabledata)
        ]


row : List HandGroupChooserViewColumnData -> Html Msg
row rowdata =
    tr
        []
        (List.map column rowdata)


column : HandGroupChooserViewColumnData -> Html Msg
column columndata =
    td
        [ class "chosercolumn"
        , style
            [ "background-color" => columndata.backgroundcolor ]
        ]
        (List.map symbol columndata.symboldatalist)


symbol : HandGroupChooserViewSymbolData -> Html Msg
symbol symboldata =
    Html.div
        [ Html.Events.onClick (GroupSelected symboldata.chooseritem)
        , onMouseDown (DragSymbol symboldata.symbol.key)
        , onDoubleClick (ReplaceSymbol symboldata.symbol.key)
        , style
            [ "margin-top" => px 3 ]
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
                (symbolsvg "hover" symboldata.symbol)
            ]
        )



--State


createhandgroupchooserdata : Choosers.Types.Model -> List (List HandGroupChooserViewColumnData)
createhandgroupchooserdata model =
    List.concat <|
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


createrowdata : Choosers.Types.Model -> List (List HandGroupChooserSubList) -> List (List HandGroupChooserViewColumnData)
createrowdata model tabledata =
    let
        filtered =
            List.filter
                (\columndata ->
                    List.length columndata > 0
                )
                tabledata

        rowdata =
            List.indexedMap
                (\row rowdata1 ->
                    createcolumndata model rowdata1 row
                )
                filtered
    in
        rowdata


createcolumndata : Choosers.Types.Model -> List HandGroupChooserSubList -> Int -> List HandGroupChooserViewColumnData
createcolumndata model rowdata row =
    (List.indexedMap
        (\col coldata ->
            let
                symboldatalist =
                    createsymboldatalist model coldata
            in
                { col = col, row = row, symboldatalist = symboldatalist, backgroundcolor = coldata.backgroundcolor }
        )
        rowdata
    )


createsymboldatalist : Choosers.Types.Model -> HandGroupChooserSubList -> List HandGroupChooserViewSymbolData
createsymboldatalist model columndata =
    List.map
        (\displayhanditem ->
            { modelmdl = model.mdl
            , symbol = displayhanditem.symbol
            , chooseritem = displayhanditem.chooseritem
            , mdlid = displayhanditem.mdlid
            }
        )
        columndata.displayhanditems



-- Prepare data for creating view data


gethandgroupchooserdata model =
    let
        basesymbol =
            String.slice 0 4 model.clicked

        choosings =
            getchoosings basesymbol model.allgroupchoosings

        rowvalues =
            List.sort <| List.Extra.unique <| List.map (\item -> item.row) choosings

        handgroupchooserdata =
            (List.map (\row -> createrowdata1 model row choosings) rowvalues)
    in
        handgroupchooserdata


createrowdata1 :
    Model
    -> Int
    -> List ChooserItem
    -> List
        (List
            { backgroundcolor : String
            , displayhanditems :
                List
                    { chooseritem : ChooserItem
                    , mdlid : Int
                    , symbol : Symbol
                    }
            }
        )
createrowdata1 model row handgroupchoosings =
    let
        rowitems =
            List.filter (\item -> item.row == row) handgroupchoosings

        colvalues =
            List.range 1 5

        featurevalues =
            List.range 1 8

        featuredata =
            List.map
                (\feature ->
                    List.filter (\d -> d.feature == feature) rowitems
                        |> converttocolumns model row colvalues
                )
                featurevalues
    in
        featuredata


converttocolumns model row colvalues items =
    filter (List.map (\col -> createcolumndata1 model row col items) colvalues)


createcolumndata1 :
    Model
    -> Int
    -> Int
    -> List ChooserItem
    -> { backgroundcolor : String
       , displayhanditems :
            List { chooseritem : ChooserItem, mdlid : Int, symbol : Symbol }
       }
createcolumndata1 model cat col choosings =
    let
        filteredhandgroupitems =
            filterhandgroupitems col (model.handgroupfilter) choosings

        backgroundcolor =
            bkcolor cat col

        displayhanditems =
            createdisplayhanditems model filteredhandgroupitems
    in
        { backgroundcolor = backgroundcolor
        , displayhanditems = displayhanditems
        }


filterhandgroupitems : Int -> Int -> List ChooserItem -> List ChooserItem
filterhandgroupitems col handgroupfilter choosings =
    case handgroupfilter of
        2 ->
            List.filter (\item -> item.col == col && item.common == False) choosings

        3 ->
            List.filter (\item -> item.col == col) choosings

        _ ->
            List.filter (\item -> item.col == col && item.common == True) choosings


createdisplayhanditems :
    Model
    -> List ChooserItem
    -> List { chooseritem : ChooserItem, mdlid : Int, symbol : Symbol }
createdisplayhanditems model items =
    List.map (createdisplayhanditem model.symbolsizes) items


createdisplayhanditem :
    Dict String Size
    -> ChooserItem
    -> { chooseritem : ChooserItem, mdlid : Int, symbol : Symbol }
createdisplayhanditem symbolsizes chooseritem =
    let
        base =
            chooseritem.base

        fill =
            2

        rotation =
            1

        symbol =
            getSymbolbyBaseFillRotation base fill rotation symbolsizes

        mdlid =
            codefromkey symbol.key + 1000
    in
        { mdlid = mdlid, symbol = symbol, chooseritem = chooseritem }


filter :
    List { b | displayhanditems : List a }
    -> List { b | displayhanditems : List a }
filter rowitems =
    let
        allempty =
            List.all (\item -> List.length item.displayhanditems == 0) rowitems

        filtered =
            List.filter (\item -> not allempty) rowitems
    in
        filtered
