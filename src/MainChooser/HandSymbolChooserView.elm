module MainChooser.HandSymbolChooserView exposing (handsymbolchooser)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.EditorSymbol exposing (..)
import MainChooser.Types exposing (..)
import SWEditor.Display exposing (signView)
import SW.Types exposing (..)
import Dict exposing (..)
import ViewHelper.ViewExtra exposing (..)
import MainChooser.HandSymbolChooser exposing (..)


handsymbolchooser : HandSymbol -> ChooserItem -> Dict String Size -> Int -> Int -> Html Msg
handsymbolchooser handsymbol choosing symbolsizes width height =
    let
        rowheight =
            truncate <| toFloat height / toFloat 10
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
                    List.append (handselection handsymbol rowheight)
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
                    (fillsview handsymbol choosing.base symbolsizes rowheight)
                ]
            , flower handsymbol rowheight
            ]


flower handsymbol rowheight =
    let
        fullwidth =
            150

        fullheight =
            fullwidth

        itemwidth =
            truncate (fullwidth / 4)

        itemheight =
            itemwidth

        radius =
            (fullwidth / 2) - (toFloat itemwidth / 2)

        centerfloating =
            truncate ((fullwidth / 2) - (sqrt (((radius) * (radius)) / 2)))

        imagesrc =
            if handsymbol.plane == Wall then
                "./img/verticalhand.png"
            else
                "./img/horizontalhand.png"
    in
        div
            [ style
                [ "width" => px fullwidth
                , "height" => px fullheight
                , "margin" => "auto"
                , "position" => "relative"
                ]
            ]
            [ div
                [ style
                    [ "position" => "absolute"
                    , "width" => px (fullwidth - (2 * itemwidth))
                    , "height" => px (fullwidth - (2 * itemwidth))
                    , "top" => px itemheight
                    , "left" => px itemwidth
                    ]
                ]
                [ img
                    [ src imagesrc
                    , style
                        [ "position" => "absolute"
                        , "width" => (mulInt (fullwidth - (2 * itemwidth)) 0.75 |> px)
                        , "margin" => "auto"
                        , "top" => px 0
                        , "bottom" => px 0
                        , "left" => px 0
                        , "right" => px 0
                        ]
                    ]
                    []
                ]
            , petaldiv
                handsymbol.flowersymbols.handfill1
                rowheight
                itemwidth
                itemheight
                0
                (centered fullwidth itemwidth)
                5
            , petaldiv
                handsymbol.flowersymbols.handfill2
                rowheight
                itemwidth
                itemheight
                (centered (centerfloating * 2) itemwidth)
                (centered (centerfloating * 2) itemheight)
                5
            , petaldiv
                handsymbol.flowersymbols.handfill3
                rowheight
                itemwidth
                itemheight
                (centered fullheight itemheight)
                0
                10
            , petaldiv
                handsymbol.flowersymbols.handfill4
                rowheight
                itemwidth
                itemheight
                (centered ((fullheight - centerfloating) * 2) itemwidth)
                (centered (centerfloating * 2) itemwidth)
                5
            , petaldiv
                handsymbol.flowersymbols.handfill5
                rowheight
                itemwidth
                itemheight
                (fullheight - itemheight)
                (centered fullwidth itemwidth)
                5
            , petaldiv
                handsymbol.flowersymbols.handfill6
                rowheight
                itemwidth
                itemheight
                (centered ((fullheight - centerfloating) * 2) itemwidth)
                (centered ((fullwidth - centerfloating) * 2) itemwidth)
                5
            , petaldiv
                handsymbol.flowersymbols.handfill7
                rowheight
                itemwidth
                itemheight
                (centered fullheight itemheight)
                (fullwidth - itemwidth)
                5
            , petaldiv
                handsymbol.flowersymbols.handfill8
                rowheight
                itemwidth
                itemheight
                (centered (centerfloating * 2) itemwidth)
                (centered ((fullwidth - centerfloating) * 2) itemwidth)
                10
            ]


mulInt : Int -> Float -> Int
mulInt num1 num2 =
    truncate (toFloat num1 * num2)



-- mulInt 5 0.75


petaldiv handfill rowheight width height top left paddingtop =
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
        [ petal handfill rowheight
        ]


petal handfill rowheight =
    symbolcentered True handfill.symbol handfill.symbol.width handfill.symbol.height


centered : Int -> Int -> Int
centered full item =
    toFloat full
        / 2
        - toFloat item
        / 2
        |> truncate


fillsview handsymbol base symbolsizes rowheight =
    let
        handfillitems =
            gethandfillitems base symbolsizes handsymbol.hand handsymbol.plane
    in
        List.map
            (\handfillitem ->
                td
                    [ onClick (SelectHandFill handfillitem.filltype)
                    , selectedbackground handfillitem.filltype handsymbol.handfill
                    ]
                    [ div
                        [ style
                            [ "position" => "relative"
                            , "display" => "block"
                            , "top" => px -12
                            , "left" => px 0
                            ]
                        ]
                        [ symbolcentered False handfillitem.symbol 50 rowheight ]
                    ]
            )
            handfillitems


handselection handsymbol rowheight =
    [ td [ onClick (SelectHand Left), selectedbackground Left handsymbol.hand ]
        [ div
            [ style
                [ "position" => "relative"
                , "display" => "block"
                , "top" => px -15
                , "left" => px 0
                ]
            ]
            [ symbolcentered False handsymbol.symbollefthand 50 rowheight ]
        , div [] [ text "Left" ]
        ]
    , td [ onClick (SelectHand Right), selectedbackground Right handsymbol.hand ]
        [ div
            [ style
                [ "position" => "relative"
                , "display" => "block"
                , "top" => px -15
                , "left" => px 0
                ]
            ]
            [ symbolcentered False handsymbol.symbolrighthand 50 rowheight ]
        , div [] [ text "Right" ]
        ]
    ]


planeselection handsymbol =
    [ td [ onClick (SelectPlane Wall), selectedbackground Wall handsymbol.plane ] [ img [ src "./img/wallplane.png", width 70 ] [], div [] [ text "Wall" ] ]
    , td [ onClick (SelectPlane Floor), selectedbackground Floor handsymbol.plane ] [ img [ src "./img/floorplane.png", width 70 ] [], div [] [ text "Floor" ] ]
    ]


selectedbackground : a -> a -> Attribute b
selectedbackground expected currentselected =
    if expected == currentselected then
        style [ "background" => "#7b85c0" ]
    else
        style []


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
            [ style
                [ "position" => "absolute"
                , "inline" => "block"
                , "margin" => "auto"
                , "width" => px width
                , "height" => px height
                ]
            , if drag then
                onMouseDown (DragSymbol symbol.code)
              else
                onMouseDown Noop
            ]
            [ App.map SignView
                (SWEditor.Display.symbolView1 "" symbol)
            ]
        ]
