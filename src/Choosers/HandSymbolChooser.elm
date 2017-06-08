module Choosers.HandSymbolChooser exposing (handsymbolchooser, wallplaneimg, floorplaneimg, gethandfillitems, createflowersymbols)

import Choosers.Types
    exposing
        ( Model
        , Msg(Editor, SelectHandFill, SignView, SelectHand, SelectPlane)
        , HandSymbol
        , HandItem
        , HandFillItem
        , Petal
        )
import Choosers.Types as Editor exposing (Editor)
import Choosers.Types as Hands exposing (Hands)
import Choosers.Types as HandFills exposing (HandFills)
import Choosers.Types as Planes exposing (Planes)
import SWEditor.EditorSymbol exposing (getSymbolbyBaseFillRotation)
import SWEditor.DisplaySvg exposing (symbolsvg)
import SW.Types exposing (Symbol, Base, Size, symbolinit)
import Dict exposing (Dict)
import Choosers.HandPng exposing (handpngspan, gethandpng)
import Choosers.Petalhelper exposing (getoutersymbolpetals)
import Choosers.CompassRose exposing (compassrosediv)


--View

import Html exposing (Html, Attribute, div, table, tr, td, text, img)
import Html.Attributes exposing (style, class, attribute, src, width)
import Html.Events exposing (onClick, onMouseDown, onDoubleClick)
import Helpers.ViewExtra exposing (px, (=>))
import List.Extra


handsymbolchooser : { a | handsymbol : HandSymbol } -> Int -> { display : Html Choosers.Types.Msg, width : Int, height : Int }
handsymbolchooser model chooserwidth =
    let
        rowheight =
            54

        fullheight =
            230

        fullwidth =
            fullheight

        outeritemwidth =
            50

        outeritemheight =
            outeritemwidth

        handsymbol =
            model.handsymbol

        rosecenterpetaldata =
            List.map (\fs -> handpngpetal fs.handpng) handsymbol.flowersymbols

        outerpetalsymbols =
            List.map (\fs -> fs.symbol) handsymbol.flowersymbols

        outersymbolpetals =
            getoutersymbolpetals outerpetalsymbols outeritemwidth outeritemheight
    in
        { display =
            div [ attribute "ondragstart" "return false;", attribute "ondrop" "return false;" ]
                [ table
                    [ class "symbolchooserheader"
                    , Html.Attributes.style
                        [ "width" => px (chooserwidth - 12)
                        , "height" => px rowheight
                        ]
                    ]
                    [ tr [] <|
                        List.append (handselectionboth handsymbol)
                            (planeselection handsymbol)
                    ]
                , table
                    [ class "symbolchooserheader"
                    , Html.Attributes.style
                        [ "width" => "100%"
                        , "height" => px rowheight
                        ]
                    ]
                    [ tr []
                        (fillsview handsymbol)
                    ]
                , compassrose
                    handsymbol.handfill
                    rosecenterpetaldata
                    outersymbolpetals
                    fullwidth
                    fullheight
                    outeritemwidth
                    outeritemheight
                ]
        , width = chooserwidth - 12
        , height = rowheight * 2 + fullheight + 10
        }


fillsview : HandSymbol -> List (Html Msg)
fillsview handsymbol =
    List.map
        (\( handfillitem, description ) ->
            td
                [ onClick (SelectHandFill handfillitem.filltype)
                , onMouseDown ((Editor << Editor.DragSymbol) handfillitem.symbol.key)
                , onDoubleClick ((Editor << Editor.ReplaceSymbol) handfillitem.symbol.key)
                , selectedbackground handfillitem.filltype handsymbol.handfill
                ]
                [ div
                    []
                    [ Html.map SignView
                        (symbolsvg "hover" handfillitem.symbol)
                    ]
                , div
                    [ style
                        [ "clear" => "both"
                        ]
                    ]
                    [ text description ]
                ]
        )
        (List.reverse
            (List.Extra.zip
                handsymbol.handfillitems
                [ "Baby Edge", "Palm", "Thumb Edge", "Back" ]
            )
        )


handselectionboth : HandSymbol -> List (Html Msg)
handselectionboth handsymbol =
    [ handselection handsymbol Hands.Left .symbollefthand "Left"
    , handselection handsymbol Hands.Right .symbolrighthand "Right"
    ]


handselection :
    { a | hand : Hands }
    -> Hands
    -> ({ a | hand : Hands } -> Symbol)
    -> String
    -> Html Msg
handselection handsymbol handType symbolgetter label =
    td
        [ onClick (SelectHand handType)
        , selectedbackground handType handsymbol.hand
        ]
        [ div
            []
            [ Html.map SignView
                (symbolsvg "hover" (symbolgetter handsymbol))
            ]
        , div
            [ style
                [ "clear" => "both"
                ]
            ]
            [ text label ]
        ]


pngfolder : String
pngfolder =
    "./assets/img/"


planeselection : { a | plane : Planes } -> List (Html Msg)
planeselection handsymbol =
    [ td
        [ onClick (SelectPlane Planes.Wall), selectedbackground Planes.Wall handsymbol.plane ]
        [ wallplaneimg
        , div [] [ text "Wall" ]
        ]
    , td
        [ onClick (SelectPlane Planes.Floor), selectedbackground Planes.Floor handsymbol.plane ]
        [ floorplaneimg
        , div [] [ text "Floor" ]
        ]
    ]


wallplaneimg : Html msg
wallplaneimg =
    img [ src <| pngfolder ++ "wallplanesmall.png", width 50 ] []


floorplaneimg : Html msg
floorplaneimg =
    img [ src <| pngfolder ++ "floorplanesmall.png", width 50 ] []


selectedbackground : a -> a -> Attribute b
selectedbackground expected currentselected =
    if expected == currentselected then
        style [ "background" => "#7b85c0" ]
    else
        style []


compassrose : HandFills -> List (Html msg) -> List (Html msg) -> Int -> Int -> Int -> Int -> Html msg
compassrose handfill rosecenterpetaldata petalcontent fullwidth fullheight outeritemwidth outeritemheight =
    let
        innersize =
            truncate <| toFloat fullwidth * 0.6

        rosecenterimagehands =
            if handfill == HandFills.LeftBabyEdge || handfill == HandFills.RightBabyEdge then
                text ""
            else
                handimagecenter rosecenterpetaldata fullwidth innersize
    in
        div
            [ style
                [ "position" => "relative"
                , "width" => px fullwidth
                , "margin" => "auto"
                ]
            ]
            [ compassrosediv fullwidth fullheight outeritemwidth outeritemheight innersize petalcontent rosecenterimagehands
            ]


handimagecenter : List (Html msg) -> Int -> Int -> Html msg
handimagecenter petalcontent parentsize fullwidth =
    let
        itemwidth =
            truncate (toFloat fullwidth / 4) + 10
    in
        div
            [ style
                [ "position" => "relative"
                , "width" => "100%"
                , "height" => "100%"
                ]
            ]
            [ compassrosediv fullwidth fullwidth itemwidth itemwidth 10 petalcontent (text "")
            ]



--State


createflowersymbols : HandSymbol -> Base -> Dict String Size -> List Petal
createflowersymbols handsymbol base symbolsizes =
    let
        selectedhandfillitem =
            gethandfillitem base symbolsizes handsymbol.plane handsymbol.handfill
    in
        if selectedhandfillitem.rotation == 1 then
            List.map (createpetal symbolsizes base selectedhandfillitem) [ 1, 2, 3, 4, 5, 6, 7, 8 ]
        else
            List.map (createpetal symbolsizes base selectedhandfillitem) [ 1, 8, 7, 6, 5, 4, 3, 2 ]


handpngpetal :
    { a | miror : Bool, pngcss : String, rotate : number }
    -> Html c
handpngpetal handpng =
    div
        [ class "centerdivcontainer"
        ]
        [ handpngspan handpng "centerdiv" "" "scale(0.8)" "inline-block" ]


gethandfillitems : Base -> Dict.Dict String Size -> Hands -> Planes -> List HandFillItem
gethandfillitems base symbolsizes handtype planetype =
    List.map (gethandfillitem base symbolsizes planetype)
        (if handtype == Hands.Right then
            [ HandFills.RightBabyEdge
            , HandFills.RightPalm
            , HandFills.RightThumbEdge
            , HandFills.RightBack
            ]
         else
            [ HandFills.LeftBabyEdge
            , HandFills.LeftPalm
            , HandFills.LeftThumbEdge
            , HandFills.LeftBack
            ]
        )


gethandfillitem : Base -> Dict String Size -> Planes -> HandFills -> HandFillItem
gethandfillitem base symbolsizes planetype handfilltype =
    List.filter (\hf -> hf.filltype == handfilltype && hf.planetype == planetype) (handfillitems base symbolsizes)
        |> List.head
        |> Maybe.withDefault { fill = 0, rotation = 0, filltype = handfilltype, planetype = planetype, symbol = symbolinit }


handfillitems : Base -> Dict String Size -> List HandFillItem
handfillitems base symbolsizes =
    List.map (createhandfillitem base symbolsizes)
        [ { fill = 2, rotation = 9, filltype = HandFills.RightBabyEdge, planetype = Planes.Wall }
        , { fill = 1, rotation = 1, filltype = HandFills.RightPalm, planetype = Planes.Wall }
        , { fill = 2, rotation = 1, filltype = HandFills.RightThumbEdge, planetype = Planes.Wall }
        , { fill = 3, rotation = 1, filltype = HandFills.RightBack, planetype = Planes.Wall }
        , { fill = 2, rotation = 1, filltype = HandFills.LeftBabyEdge, planetype = Planes.Wall }
        , { fill = 1, rotation = 9, filltype = HandFills.LeftPalm, planetype = Planes.Wall }
        , { fill = 2, rotation = 9, filltype = HandFills.LeftThumbEdge, planetype = Planes.Wall }
        , { fill = 3, rotation = 9, filltype = HandFills.LeftBack, planetype = Planes.Wall }
        , { fill = 5, rotation = 9, filltype = HandFills.RightBabyEdge, planetype = Planes.Floor }
        , { fill = 4, rotation = 1, filltype = HandFills.RightPalm, planetype = Planes.Floor }
        , { fill = 5, rotation = 1, filltype = HandFills.RightThumbEdge, planetype = Planes.Floor }
        , { fill = 6, rotation = 1, filltype = HandFills.RightBack, planetype = Planes.Floor }
        , { fill = 5, rotation = 1, filltype = HandFills.LeftBabyEdge, planetype = Planes.Floor }
        , { fill = 4, rotation = 9, filltype = HandFills.LeftPalm, planetype = Planes.Floor }
        , { fill = 5, rotation = 9, filltype = HandFills.LeftThumbEdge, planetype = Planes.Floor }
        , { fill = 6, rotation = 9, filltype = HandFills.LeftBack, planetype = Planes.Floor }
        ]


createhandfillitem : Base -> Dict String Size -> HandItem -> HandFillItem
createhandfillitem base symbolsizes basefillitem =
    let
        symbol =
            getSymbolbyBaseFillRotation base basefillitem.fill basefillitem.rotation symbolsizes
    in
        { fill = basefillitem.fill
        , rotation = basefillitem.rotation
        , filltype = basefillitem.filltype
        , planetype = basefillitem.planetype
        , symbol = symbol
        }


createpetal : Dict String Size -> Base -> HandFillItem -> Int -> Petal
createpetal symbolsizes base selectedhandfillitem rotationoffset =
    let
        finalrotationoffset =
            rotationoffset - 1

        rotation =
            selectedhandfillitem.rotation + finalrotationoffset

        symbol =
            getSymbolbyBaseFillRotation base selectedhandfillitem.fill rotation symbolsizes

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
