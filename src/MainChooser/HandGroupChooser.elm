module MainChooser.HandGroupChooser exposing (..)

import MainChooser.Types exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SW.Types exposing (..)
import Dict exposing (..)
import String exposing (..)
import List.Extra exposing (..)


gethandgroupchooserdata model =
    let
        basesymbol =
            String.slice 0 4 model.clicked

        choosings =
            getchoosings basesymbol model.allgroupchoosings

        rowvalues =
            List.sort <| List.Extra.unique <| List.map (\item -> item.row) choosings

        handgroupchooserdata =
            (List.map (\row -> createrowdata model row choosings) rowvalues)
    in
        handgroupchooserdata


createrowdata :
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
                    , symbol : EditorSymbol
                    }
            }
        )
createrowdata model row handgroupchoosings =
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
    filter (List.map (\col -> createcolumndata model row col items) colvalues)


createcolumndata :
    Model
    -> Int
    -> Int
    -> List ChooserItem
    -> { backgroundcolor : String
       , displayhanditems :
            List { chooseritem : ChooserItem, mdlid : Int, symbol : EditorSymbol }
       }
createcolumndata model cat col choosings =
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
    -> List { chooseritem : ChooserItem, mdlid : Int, symbol : EditorSymbol }
createdisplayhanditems model items =
    List.map (createdisplayhanditem model.symbolsizes) items


createdisplayhanditem :
    Dict String Size
    -> ChooserItem
    -> { chooseritem : ChooserItem, mdlid : Int, symbol : EditorSymbol }
createdisplayhanditem symbolsizes chooseritem =
    let
        base =
            chooseritem.base

        fill =
            2

        rotation =
            1

        symbol =
            getSymbolEditorBaseFillRotation base fill rotation symbolsizes

        mdlid =
            symbol.code + 1000
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
