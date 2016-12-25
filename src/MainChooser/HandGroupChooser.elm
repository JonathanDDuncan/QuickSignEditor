module MainChooser.HandGroupChooser exposing (..)

import SWEditor.EditorSymbol exposing (..)
import Exts.List exposing (..)


gethandgroupchooserdata model handgroupchoosings =
    let
        rowvalues =
            List.sort <| unique <| List.map (\item -> item.row) handgroupchoosings

        handgroupchooserdata =
            (List.map (\row -> createrowdata model row handgroupchoosings) rowvalues)
    in
        handgroupchooserdata


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


filterhandgroupitems col handgroupfilter choosings =
    case handgroupfilter of
        2 ->
            List.filter (\item -> item.col == col && item.common == False) choosings

        3 ->
            List.filter (\item -> item.col == col) choosings

        _ ->
            List.filter (\item -> item.col == col && item.common == True) choosings


createdisplayhanditems model items =
    List.map (createdisplayhanditem model.symbolsizes) items


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
