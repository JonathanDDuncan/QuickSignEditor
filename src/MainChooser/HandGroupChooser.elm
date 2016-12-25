module MainChooser.HandGroupChooser exposing (..)

import MainChooser.Types exposing (..)
import SWEditor.EditorSymbol exposing (..)
import Exts.List exposing (..)
import SW.Types exposing (..)
import Dict exposing (..)
import String exposing (..)


gethandgroupchooserdata :
    Model
    -> List HandGroupChooser
gethandgroupchooserdata model =
    let
        basesymbol =
            String.slice 0 4 model.clicked

        choosings =
            getchoosings basesymbol model.allgroupchoosings

        rowvalues =
            List.sort <| unique <| List.map (\item -> item.row) choosings

        handgroupchooserdata =
            (List.map (\row -> createrowdata model row choosings) rowvalues)
    in
        handgroupchooserdata


createrowdata :
    Model
    -> Int
    -> List ChooserItem
    -> { datawithoutthumbs :
            List
                { backgroundcolor : String
                , displayhanditems :
                    List
                        { chooseritem : ChooserItem
                        , mdlid : Int
                        , symbol : EditorSymbol
                        }
                }
       , datawiththumbs :
            List
                { backgroundcolor : String
                , displayhanditems :
                    List
                        { chooseritem : ChooserItem
                        , mdlid : Int
                        , symbol : EditorSymbol
                        }
                }
       }
createrowdata model row handgroupchoosings =
    let
        rowitems =
            List.filter (\item -> item.row == row) handgroupchoosings

        withoutthumbs =
            List.filter (\item -> not item.thumb) rowitems

        withthumbs =
            List.filter (\item -> item.thumb) rowitems

        colvalues =
            [1..5]

        datawithoutthumbs =
            filter (List.map (\col -> createcolumndata model row col withoutthumbs) colvalues)

        datawiththumbs =
            filter (List.map (\col -> createcolumndata model row col withthumbs) colvalues)
    in
        { datawithoutthumbs = datawithoutthumbs, datawiththumbs = datawiththumbs }


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
            filterhandgroupitems col model.handgroupfilter choosings

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
