module MainChooser.HandSymbolChooser exposing (..)

import MainChooser.Types exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SW.Types exposing (..)
import Dict exposing (..)
import String exposing (..)


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


gethandfillitem : Base -> Dict String Size -> HandFills -> Planes -> HandFillItem
gethandfillitem base symbolsizes handfilltype planetype =
    List.filter (\hf -> hf.filltype == handfilltype && hf.planetype == planetype) (handfillitems base symbolsizes)
        |> List.head
        |> Maybe.withDefault { fill = 0, rotation = 0, filltype = handfilltype, planetype = planetype, symbol = symbolinit }


handfillitems : Base -> Dict String Size -> List HandFillItem
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


createhandfillitem : Base -> Dict String Size -> HandItem -> HandFillItem
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


calcpetal : Dict String Size -> Base -> HandFillItem -> Int -> Petal
calcpetal symbolsizes base selectedhandfillitem rotationoffset =
    let
        finalrotationoffset =
            rotationoffset - 1

        rotation =
            (selectedhandfillitem.rotation + finalrotationoffset)

        symbol =
            getSymbolEditorBaseFillRotation base selectedhandfillitem.fill rotation symbolsizes

        handpng =
            gethandpng symbol.key rotation selectedhandfillitem.fill selectedhandfillitem.filltype
    in
        { fill = selectedhandfillitem.fill
        , rotation = selectedhandfillitem.rotation
        , rotationoffset = finalrotationoffset
        , filltype = selectedhandfillitem.filltype
        , planetype = selectedhandfillitem.planetype
        , symbol = symbol
        , handpng = handpng
        }


gethandpng : String -> Int -> Int -> HandFills -> HandPng
gethandpng key rotation fill filltype =
    let
        handtype =
            gethandtype filltype

        pngcss1 =
            handpngcss key fill filltype

        rotate =
            gethandpngrotation rotation handtype

        miror =
            gethandpngmiror handtype
    in
        { pngcss = pngcss1, rotate = rotate, miror = miror }


gethandpngrotation : Int -> Hands -> Int
gethandpngrotation rotation handtype =
    case handtype of
        Right ->
            case rotation of
                2 ->
                    315

                3 ->
                    270

                4 ->
                    225

                5 ->
                    180

                6 ->
                    135

                7 ->
                    90

                8 ->
                    45

                _ ->
                    0

        Left ->
            case rotation of
                2 ->
                    45

                3 ->
                    90

                4 ->
                    135

                5 ->
                    180

                6 ->
                    225

                7 ->
                    270

                8 ->
                    315

                9 ->
                    0

                10 ->
                    315

                11 ->
                    270

                12 ->
                    225

                13 ->
                    180

                14 ->
                    135

                15 ->
                    90

                16 ->
                    45

                _ ->
                    0


gethandpngmiror : Hands -> Bool
gethandpngmiror handtype =
    if handtype == Right then
        False
    else
        True


handpngcss : String -> Int -> HandFills -> String
handpngcss symbolkey fill filltype =
    if filltype == LeftBabyEdge || filltype == RightBabyEdge then
        ""
    else
        String.toLower "hands-" ++ toLower (String.slice 1 4 symbolkey) ++ toString (fill - 1) ++ "0"