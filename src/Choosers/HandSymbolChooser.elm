module Choosers.HandSymbolChooser exposing (..)

import Choosers.Types exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SWEditor.Display exposing (..)
import SW.Types exposing (..)
import Dict exposing (..)
import Choosers.HandPng exposing (..)


--View

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ViewHelper.ViewExtra exposing (..)
import Choosers.Types exposing (..)
import List.Extra exposing (..)
import SWEditor.EditorSymbol exposing (..)
import Choosers.CompassRose exposing (..)
import Choosers.HandPng exposing (..)
import Convert.ConvertFsw exposing (rotation)


handsymbolchooser : { a | handsymbol : HandSymbol } -> Int -> Int -> Html Msg
handsymbolchooser model width height =
    let
        handsymbol =
            model.handsymbol

        rosecenterpetaldata =
            List.map (\fs -> handpngpetal fs.handpng) handsymbol.flowersymbols

        roseouterpetaldata =
            List.map petal handsymbol.flowersymbols

        rowheight =
            truncate <| toFloat height / toFloat 10

        fullwidth =
            220
    in
        div [ attribute "ondragstart" "return false;", attribute "ondrop" "return false;" ]
            [ table
                [ class "symbolchooserheader"
                , Html.Attributes.style
                    [ "width" => px (width - 12)
                    , "height" => px rowheight
                    ]
                ]
                [ tr [] <|
                    List.append (handselectionboth handsymbol rowheight)
                        (planeselection handsymbol)
                ]
            , table
                [ class "symbolchooserheader"
                , Html.Attributes.style
                    [ "width" => "100%"
                    , "height" => px 50
                    ]
                ]
                [ tr []
                    (fillsview handsymbol rowheight)
                ]
            , compassrose handsymbol.handfill rosecenterpetaldata roseouterpetaldata fullwidth
            ]


fillsview : HandSymbol -> Int -> List (Html Msg)
fillsview handsymbol rowheight =
    (List.map
        (\( handfillitem, description ) ->
            td
                [ onClick (SelectHandFill handfillitem.filltype)
                , onMouseDown (DragSymbol handfillitem.symbol.code)
                , onDoubleClick (ReplaceSymbol handfillitem.symbol.code)
                , selectedbackground handfillitem.filltype handsymbol.handfill
                ]
                [ div
                    [ style
                        [ "position" => "relative"
                        , "display" => "block"
                        , "top"
                            => px
                                (6
                                    + if rotation handfillitem.symbol.code < 9 then
                                        2
                                      else
                                        0
                                )
                        , "left" => px 0
                        , "margin-bottom"
                            => (px
                                    (if (Debug.log "rotation" <| rotation handfillitem.symbol.code) >= 9 then
                                        10
                                     else
                                        0
                                    )
                               )
                        ]
                    ]
                    [ Html.map SignView
                        (symbolaloneView handfillitem.symbol 0)
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
    )


handselectionboth : HandSymbol -> Int -> List (Html Msg)
handselectionboth handsymbol rowheight =
    [ handselection handsymbol rowheight Left .symbollefthand 6 10 "Left"
    , handselection handsymbol rowheight Right .symbolrighthand 6 0 "Right"
    ]


handselection :
    { a | hand : Hands }
    -> Int
    -> Hands
    -> ({ a | hand : Hands } -> EditorSymbol)
    -> Int
    -> Int
    -> String
    -> Html Msg
handselection handsymbol rowheight handType symbolgetter topoffset marginbottom label =
    td
        [ onClick (SelectHand handType)
        , selectedbackground handType handsymbol.hand
        ]
        [ div
            [ style
                [ "position" => "relative"
                , "display" => "block"
                , "top" => px (2 + topoffset)
                , "left" => px 0
                , "margin-bottom" => px marginbottom
                ]
            ]
            [ Html.map SignView
                (symbolaloneView (symbolgetter handsymbol) 0)
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
        [ onClick (SelectPlane Wall), selectedbackground Wall handsymbol.plane ]
        [ wallplaneimg
        , div [] [ text "Wall" ]
        ]
    , td
        [ onClick (SelectPlane Floor), selectedbackground Floor handsymbol.plane ]
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


compassrose : HandFills -> List (Html msg) -> List (Html msg) -> Int -> Html msg
compassrose handfill rosecenterpetaldata petalcontent fullwidth =
    let
        fullheight =
            fullwidth

        itemwidth =
            50

        -- truncate (toFloat fullwidth / 5)
        itemheight =
            itemwidth

        innersize =
            truncate <| toFloat fullwidth * 0.6

        rosecenterimagehands =
            if (handfill == LeftBabyEdge || handfill == RightBabyEdge) then
                text ""
            else
                handimagecenter rosecenterpetaldata fullwidth itemwidth innersize
    in
        div
            [ style
                [ "position" => "relative"
                , "width" => px fullwidth
                , "margin" => "auto"
                ]
            ]
            [ compassrosediv fullwidth fullheight itemwidth itemheight 0 innersize petalcontent rosecenterimagehands
            ]


handimagecenter : List (Html msg) -> Int -> Int -> Int -> Html msg
handimagecenter petalcontent parentsize parentitemsize fullwidth =
    let
        itemwidth =
            truncate (toFloat fullwidth / 4) + 10

        top =
            Debug.log "top" <|
                truncate <|
                    toFloat (Debug.log "parentsize" parentsize - Debug.log "fullwidth" fullwidth)
                        / 2
    in
        div
            [ style
                [ "position" => "relative"
                , "width" => "100%"
                , "height" => "100%"
                ]
            ]
            [ compassrosediv fullwidth fullwidth itemwidth itemwidth top 10 petalcontent (text "")
            ]


symbolcentered : Bool -> EditorSymbol -> Int -> Int -> Html Msg
symbolcentered drag symbol width height =
    div
        [ if drag then
            onMouseDown (DragSymbol symbol.code)
          else
            onMouseDown Noop
        , onDoubleClick
            (ReplaceSymbol symbol.code)
        ]
        [ Html.map SignView
            (SWEditor.Display.symbolView1 "" symbol)
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


petal : Petal -> Html Msg
petal handfill =
    symbolcentered True handfill.symbol handfill.symbol.width handfill.symbol.height


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
        (if handtype == Right then
            [ RightBabyEdge
            , RightPalm
            , RightThumbEdge
            , RightBack
            ]
         else
            [ LeftBabyEdge
            , LeftPalm
            , LeftThumbEdge
            , LeftBack
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
        [ { fill = 2, rotation = 9, filltype = RightBabyEdge, planetype = Wall }
        , { fill = 1, rotation = 1, filltype = RightPalm, planetype = Wall }
        , { fill = 2, rotation = 1, filltype = RightThumbEdge, planetype = Wall }
        , { fill = 3, rotation = 1, filltype = RightBack, planetype = Wall }
        , { fill = 2, rotation = 1, filltype = LeftBabyEdge, planetype = Wall }
        , { fill = 1, rotation = 9, filltype = LeftPalm, planetype = Wall }
        , { fill = 2, rotation = 9, filltype = LeftThumbEdge, planetype = Wall }
        , { fill = 3, rotation = 9, filltype = LeftBack, planetype = Wall }
        , { fill = 5, rotation = 9, filltype = RightBabyEdge, planetype = Floor }
        , { fill = 4, rotation = 1, filltype = RightPalm, planetype = Floor }
        , { fill = 5, rotation = 1, filltype = RightThumbEdge, planetype = Floor }
        , { fill = 6, rotation = 1, filltype = RightBack, planetype = Floor }
        , { fill = 5, rotation = 1, filltype = LeftBabyEdge, planetype = Floor }
        , { fill = 4, rotation = 9, filltype = LeftPalm, planetype = Floor }
        , { fill = 5, rotation = 9, filltype = LeftThumbEdge, planetype = Floor }
        , { fill = 6, rotation = 9, filltype = LeftBack, planetype = Floor }
        ]


createhandfillitem : Base -> Dict String Size -> HandItem -> HandFillItem
createhandfillitem base symbolsizes basefillitem =
    let
        symbol =
            getSymbolEditorBaseFillRotation base basefillitem.fill basefillitem.rotation symbolsizes
    in
        { fill = basefillitem.fill, rotation = basefillitem.rotation, filltype = basefillitem.filltype, planetype = basefillitem.planetype, symbol = symbol }


createpetal : Dict String Size -> Base -> HandFillItem -> Int -> Petal
createpetal symbolsizes base selectedhandfillitem rotationoffset =
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