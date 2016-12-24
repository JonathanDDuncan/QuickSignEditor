module MainChooser.HandSymbolChooser exposing (..)

import MainChooser.Types exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SW.Types exposing (..)
import Dict exposing (..)


gethandfillitems : Base -> Dict.Dict String Size -> Hands -> Planes -> List HandFillItem
gethandfillitems base symbolsizes handtype plantype =
    if handtype == Right then
        [ gethandfillitem base symbolsizes RightBabyEdge plantype
        , gethandfillitem base symbolsizes RightPalm plantype
        , gethandfillitem base symbolsizes RightThumbEdge plantype
        , gethandfillitem base symbolsizes RightBack plantype
        ]
    else
        [ gethandfillitem base symbolsizes LeftBabyEdge plantype
        , gethandfillitem base symbolsizes LeftPalm plantype
        , gethandfillitem base symbolsizes LeftThumbEdge plantype
        , gethandfillitem base symbolsizes LeftBack plantype
        ]


gethandfillitem base symbolsizes handfilltype planetype =
    List.filter (\hf -> hf.filltype == handfilltype && hf.planetype == planetype) (handfillitems base symbolsizes)
        |> List.head
        |> Maybe.withDefault { fill = 0, rotation = 0, filltype = handfilltype, planetype = planetype, symbol = symbolinit }


handfillitems base symbolsizes =
    [ createhandfillitem base symbolsizes { fill = 2, rotation = 9, filltype = RightBabyEdge, planetype = Wall }
    , createhandfillitem base symbolsizes { fill = 1, rotation = 1, filltype = RightPalm, planetype = Wall }
    , createhandfillitem base symbolsizes { fill = 2, rotation = 1, filltype = RightThumbEdge, planetype = Wall }
    , createhandfillitem base symbolsizes { fill = 3, rotation = 1, filltype = RightBack, planetype = Wall }
    , createhandfillitem base symbolsizes { fill = 2, rotation = 1, filltype = LeftBabyEdge, planetype = Wall }
    , createhandfillitem base symbolsizes { fill = 1, rotation = 1, filltype = LeftPalm, planetype = Wall }
    , createhandfillitem base symbolsizes { fill = 2, rotation = 9, filltype = LeftThumbEdge, planetype = Wall }
    , createhandfillitem base symbolsizes { fill = 3, rotation = 1, filltype = LeftBack, planetype = Wall }
    , createhandfillitem base symbolsizes { fill = 5, rotation = 9, filltype = RightBabyEdge, planetype = Floor }
    , createhandfillitem base symbolsizes { fill = 4, rotation = 1, filltype = RightPalm, planetype = Floor }
    , createhandfillitem base symbolsizes { fill = 5, rotation = 1, filltype = RightThumbEdge, planetype = Floor }
    , createhandfillitem base symbolsizes { fill = 6, rotation = 1, filltype = RightBack, planetype = Floor }
    , createhandfillitem base symbolsizes { fill = 5, rotation = 1, filltype = LeftBabyEdge, planetype = Floor }
    , createhandfillitem base symbolsizes { fill = 4, rotation = 1, filltype = LeftPalm, planetype = Floor }
    , createhandfillitem base symbolsizes { fill = 5, rotation = 9, filltype = LeftThumbEdge, planetype = Floor }
    , createhandfillitem base symbolsizes { fill = 6, rotation = 1, filltype = LeftBack, planetype = Floor }
    ]


createhandfillitem base symbolsizes basefillitem =
    let
        symbol =
            getSymbolEditorBaseFillRotation base basefillitem.fill basefillitem.rotation symbolsizes
    in
        { fill = basefillitem.fill, rotation = basefillitem.rotation, filltype = basefillitem.filltype, planetype = basefillitem.planetype, symbol = symbol }


getpetals :
    { a | handfill : HandFills, plane : Planes }
    -> Base
    -> Dict String Size
    -> Flower
getpetals handsymbol base symbolsizes =
    let
        selectedhandfillitem =
            gethandfillitem base symbolsizes handsymbol.handfill handsymbol.plane
    in
        if selectedhandfillitem.rotation == 1 then
            { handfill1 = calcpetal symbolsizes base selectedhandfillitem 1
            , handfill2 = calcpetal symbolsizes base selectedhandfillitem 2
            , handfill3 = calcpetal symbolsizes base selectedhandfillitem 3
            , handfill4 = calcpetal symbolsizes base selectedhandfillitem 4
            , handfill5 = calcpetal symbolsizes base selectedhandfillitem 5
            , handfill6 = calcpetal symbolsizes base selectedhandfillitem 6
            , handfill7 = calcpetal symbolsizes base selectedhandfillitem 7
            , handfill8 = calcpetal symbolsizes base selectedhandfillitem 8
            }
        else
            { handfill1 = calcpetal symbolsizes base selectedhandfillitem 1
            , handfill2 = calcpetal symbolsizes base selectedhandfillitem 8
            , handfill3 = calcpetal symbolsizes base selectedhandfillitem 7
            , handfill4 = calcpetal symbolsizes base selectedhandfillitem 6
            , handfill5 = calcpetal symbolsizes base selectedhandfillitem 5
            , handfill6 = calcpetal symbolsizes base selectedhandfillitem 4
            , handfill7 = calcpetal symbolsizes base selectedhandfillitem 3
            , handfill8 = calcpetal symbolsizes base selectedhandfillitem 2
            }


calcpetal symbolsizes base selectedhandfillitem rotationoffset =
    let
        finalrotationoffset =
            rotationoffset - 1

        symbol =
            getSymbolEditorBaseFillRotation base selectedhandfillitem.fill (selectedhandfillitem.rotation + finalrotationoffset) symbolsizes
    in
        { fill = selectedhandfillitem.fill, rotation = selectedhandfillitem.rotation, rotationoffset = finalrotationoffset, filltype = selectedhandfillitem.filltype, planetype = selectedhandfillitem.planetype, symbol = symbol }
