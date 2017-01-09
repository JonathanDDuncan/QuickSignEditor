module MainChooser.CompassRoseView exposing (compassrose, symbolcentered)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
 
import ViewHelper.ViewExtra exposing (..)
import SWEditor.EditorSymbol exposing (..)
import MainChooser.Types exposing (..)
import SWEditor.Display exposing (signView)
import MainChooser.HandPng exposing (..)


compassrose handsymbol fullwidth =
    let
        fullheight =
            fullwidth

        itemwidth =
            truncate (toFloat fullwidth / 4)

        itemheight =
            itemwidth

        petals =
            { petal1 = petal handsymbol.flowersymbols.handfill1
            , petal2 = petal handsymbol.flowersymbols.handfill2
            , petal3 = petal handsymbol.flowersymbols.handfill3
            , petal4 = petal handsymbol.flowersymbols.handfill4
            , petal5 = petal handsymbol.flowersymbols.handfill5
            , petal6 = petal handsymbol.flowersymbols.handfill6
            , petal7 = petal handsymbol.flowersymbols.handfill7
            , petal8 = petal handsymbol.flowersymbols.handfill8
            }

        rosecenterimagehands =
            if (handsymbol.handfill == LeftBabyEdge || handsymbol.handfill == RightBabyEdge) then
                text ""
            else
                handimagecenter handsymbol fullwidth itemwidth
    in
        compassrosediv fullwidth fullheight itemwidth itemheight petals rosecenterimagehands 0


handimagecenter handsymbol parentsize parentitemsize =
    let
        fullwidth =
            truncate (toFloat parentsize - (toFloat parentitemsize * 1.75))

        itemwidth =
            truncate (toFloat fullwidth / 4) + 10

        petals =
            { petal1 = handpngpetal handsymbol.flowersymbols.handfill1.handpng
            , petal2 = handpngpetal handsymbol.flowersymbols.handfill2.handpng
            , petal3 = handpngpetal handsymbol.flowersymbols.handfill3.handpng
            , petal4 = handpngpetal handsymbol.flowersymbols.handfill4.handpng
            , petal5 = handpngpetal handsymbol.flowersymbols.handfill5.handpng
            , petal6 = handpngpetal handsymbol.flowersymbols.handfill6.handpng
            , petal7 = handpngpetal handsymbol.flowersymbols.handfill7.handpng
            , petal8 = handpngpetal handsymbol.flowersymbols.handfill8.handpng
            }

        top =
            -20
    in
        compassrosediv fullwidth fullwidth itemwidth itemwidth petals (text "") top


compassrosediv :
    Int
    -> Int
    -> Int
    -> Int
    -> { petal1 : Html Msg
       , petal2 : Html Msg
       , petal3 : Html Msg
       , petal4 : Html Msg
       , petal5 : Html Msg
       , petal6 : Html Msg
       , petal7 : Html Msg
       , petal8 : Html Msg
       }
    -> Html Msg
    -> Int
    -> Html Msg
compassrosediv fullwidth fullheight itemwidth itemheight petals rosecenter top =
    let
        radius =
            (toFloat fullwidth / 2) - (toFloat itemwidth / 2)

        centerfloating =
            truncate ((toFloat fullwidth / 2) - (sqrt (((radius) * (radius)) / 2)))

        petal1 =
            petaldiv
                itemwidth
                itemheight
                0
                (centered fullwidth itemwidth)
                5
                petals.petal1

        petal2 =
            petaldiv
                itemwidth
                itemheight
                (centered (centerfloating * 2) itemwidth)
                (centered (centerfloating * 2) itemheight)
                5
                petals.petal2

        petal3 =
            petaldiv
                itemwidth
                itemheight
                (centered fullheight itemheight)
                0
                10
                petals.petal3

        petal4 =
            petaldiv
                itemwidth
                itemheight
                (centered ((fullheight - centerfloating) * 2) itemwidth)
                (centered (centerfloating * 2) itemwidth)
                5
                petals.petal4

        petal5 =
            petaldiv
                itemwidth
                itemheight
                (fullheight - itemheight)
                (centered fullwidth itemwidth)
                5
                petals.petal5

        petal6 =
            petaldiv
                itemwidth
                itemheight
                (centered ((fullheight - centerfloating) * 2) itemwidth)
                (centered ((fullwidth - centerfloating) * 2) itemwidth)
                5
                petals.petal6

        petal7 =
            petaldiv
                itemwidth
                itemheight
                (centered fullheight itemheight)
                (fullwidth - itemwidth)
                5
                petals.petal7

        petal8 =
            petaldiv
                itemwidth
                itemheight
                (centered (centerfloating * 2) itemwidth)
                (centered ((fullwidth - centerfloating) * 2) itemwidth)
                10
                petals.petal8
    in
        div
            [ style
                [ "width" => px fullwidth
                , "height" => px fullheight
                , "margin" => "auto"
                , "position" => "relative"
                , "top" => px top
                ]
            ]
            [ div
                [ style
                    [ "position" => "absolute"
                    , "width" => px (fullwidth - (2 * itemwidth))
                    , "height" => px (fullheight - (2 * itemheight))
                    , "top" => px itemheight
                    , "left" => px itemwidth
                    ]
                ]
                [ rosecenter
                ]
            , petal1
            , petal2
            , petal3
            , petal4
            , petal5
            , petal6
            , petal7
            , petal8
            ]


petaldiv : Int -> Int -> Int -> Int -> a -> Html b -> Html b
petaldiv width height top left paddingtop display =
    div
        [ style
            [ "position" => "absolute"
            , "width" => px width
            , "height" => px height
            , "top" => px top
            , "left" => px left
            , "pading-top" => px 20
            ]
        ]
        [ display
        ]


handpngpetal :
    { a | miror : Bool, pngcss : String, rotate : number }
    -> Html c
handpngpetal handpng =
    handpngspan handpng "" "scale(0.75)"


symbolcentered : Bool -> EditorSymbol -> Int -> Int -> Html Msg
symbolcentered drag symbol width height =
    div
        [ style
            [ "position" => "relative"
            , "width" => px width
            , "height" => px height
            , "margin" => "auto"
            ]
        ]
        [ div
            [ if drag then
                onMouseDown (DragSymbol symbol.code)
              else
                onMouseDown Noop
            ]
            [ Html.map SignView
                (SWEditor.Display.symbolView1 "" symbol)
            ]
        ]


petal handfill =
    symbolcentered True handfill.symbol handfill.symbol.width handfill.symbol.height


centered : Int -> Int -> Int
centered full item =
    toFloat full
        / 2
        - toFloat item
        / 2
        |> truncate


mulInt : Int -> Float -> Int
mulInt num1 num2 =
    truncate (toFloat num1 * num2)



-- mulInt 5 0.75
