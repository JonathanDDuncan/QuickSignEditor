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
                    List.append (handselection handsymbol choosing.base symbolsizes rowheight)
                        (planeselection handsymbol choosing.base symbolsizes rowheight)
                ]
            , table
                [ class "symbolchooserheader"
                , Html.Attributes.style
                    [ "width" => px (width - 12)
                    , "height" => px rowheight
                    ]
                ]
                [ tr [] (fillsview handsymbol choosing.base symbolsizes rowheight)
                ]
            , flower handsymbol choosing.base symbolsizes rowheight
            ]


flower handsymbol base symbolsizes rowheight =
    let
        fullwidth =
            240

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

        flowersymbols =
            getpetals handsymbol
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
                    , "width" => (px <| (fullwidth - (2 * itemwidth)))
                    , "height" => (px <| (fullheight - (2 * itemheight)))
                    , "top" => px (itemwidth - 12)
                    , "left" => px itemwidth
                    ]
                ]
                [ img
                    [ src imagesrc
                    , style
                        [ "position" => "absolute"
                        , "width" => px (fullwidth - (2 * itemwidth))
                        , "top" => px 0
                        , "bottom" => px 0
                        , "margin" => "auto"
                        ]
                    ]
                    []
                ]
            , petaldiv
                flowersymbols.handfill1
                base
                symbolsizes
                rowheight
                itemwidth
                itemheight
                0
                (centered fullwidth itemwidth)
            , petaldiv
                flowersymbols.handfill2
                base
                symbolsizes
                rowheight
                itemwidth
                itemheight
                (centered (centerfloating * 2) itemwidth)
                (centered (centerfloating * 2) itemheight)
            , petaldiv
                flowersymbols.handfill3
                base
                symbolsizes
                rowheight
                itemwidth
                itemheight
                (centered fullheight itemheight)
                0
            , petaldiv
                flowersymbols.handfill4
                base
                symbolsizes
                rowheight
                itemwidth
                itemheight
                (centered ((fullheight - centerfloating) * 2) itemwidth)
                (centered (centerfloating * 2) itemwidth)
            , petaldiv
                flowersymbols.handfill5
                base
                symbolsizes
                rowheight
                itemwidth
                itemheight
                (fullheight - itemheight)
                (centered fullwidth itemwidth)
            , petaldiv
                flowersymbols.handfill6
                base
                symbolsizes
                rowheight
                itemwidth
                itemheight
                (centered ((fullheight - centerfloating) * 2) itemwidth)
                (centered ((fullwidth - centerfloating) * 2) itemwidth)
            , petaldiv
                flowersymbols.handfill7
                base
                symbolsizes
                rowheight
                itemwidth
                itemheight
                (centered fullheight itemheight)
                (fullwidth - itemwidth)
            , petaldiv
                flowersymbols.handfill7
                base
                symbolsizes
                rowheight
                itemwidth
                itemheight
                (centered (centerfloating * 2) itemwidth)
                (centered ((fullwidth - centerfloating) * 2) itemwidth)
            ]


petaldiv : { a | fill : Fill, increment : Int, rotation : Int } -> Base -> Dict String Size -> b -> Int -> Int -> Int -> Int -> Html Msg
petaldiv handfill base symbolsizes rowheight width height top left =
    div
        [ style
            [ "position" => "absolute"
            , "width" => px width
            , "height" => px height
            , "top" => px top
            , "left" => px left
            ]
        ]
        [ petal handfill base symbolsizes rowheight ]


petal : { a | fill : Fill, rotation : Int, increment : Int } -> Base -> Dict String Size -> b -> Html Msg
petal handfill base symbolsizes rowheight =
    let
        symbol =
            getSymbolEditorBaseFillRotation base handfill.fill (handfill.rotation + handfill.increment) symbolsizes
    in
        symbolcentered True base handfill.fill (handfill.rotation + handfill.increment) symbolsizes 50 50


getpetals handsymbol =
    let
        handfill =
            gethandfill handsymbol.handfill
    in
        if handfill.rotation == 1 then
            { handfill1 = rotationincrement handfill 1
            , handfill2 = rotationincrement handfill 2
            , handfill3 = rotationincrement handfill 3
            , handfill4 = rotationincrement handfill 4
            , handfill5 = rotationincrement handfill 5
            , handfill6 = rotationincrement handfill 6
            , handfill7 = rotationincrement handfill 7
            , handfill8 = rotationincrement handfill 8
            }
        else
            { handfill1 = rotationincrement handfill 1
            , handfill2 = rotationincrement handfill 8
            , handfill3 = rotationincrement handfill 7
            , handfill4 = rotationincrement handfill 6
            , handfill5 = rotationincrement handfill 5
            , handfill6 = rotationincrement handfill 4
            , handfill7 = rotationincrement handfill 3
            , handfill8 = rotationincrement handfill 2
            }


rotationincrement : { d | fill : a, handfill : b, rotation : c } -> number -> { fill : a, handfill : b, increment : number, rotation : c }
rotationincrement handfill increment =
    { fill = handfill.fill, rotation = handfill.rotation, increment = increment - 1, handfill = handfill.handfill }


centered : Int -> Int -> Int
centered full item =
    toFloat full
        / 2
        - toFloat item
        / 2
        |> truncate


fillsview : HandSymbol -> Base -> Dict String Size -> Int -> List (Html Msg)
fillsview handsymbol base symbolsizes rowheight =
    let
        handfills =
            if handsymbol.hand == Right then
                [ gethandfill RightBabyEdge
                , gethandfill RightPalm
                , gethandfill RightThumbEdge
                , gethandfill RightBack
                ]
            else
                [ gethandfill LeftBabyEdge
                , gethandfill LeftPalm
                , gethandfill LeftThumbEdge
                , gethandfill LeftBack
                ]
    in
        List.map
            (\handfill ->
                td
                    [ onClick (SelectHandFill handfill.handfill)
                    , selectedbackground handfill.handfill handsymbol.handfill
                    ]
                    [ symbolcentered False base handfill.fill handfill.rotation symbolsizes 50 rowheight
                    ]
            )
            handfills


gethandfill handfill =
    List.filter (\hf -> hf.handfill == handfill) handfills
        |> List.head
        |> Maybe.withDefault { fill = 0, rotation = 0, handfill = handfill }


handfills =
    [ { fill = 2, rotation = 9, handfill = RightBabyEdge }
    , { fill = 1, rotation = 1, handfill = RightPalm }
    , { fill = 2, rotation = 1, handfill = RightThumbEdge }
    , { fill = 3, rotation = 1, handfill = RightBack }
    , { fill = 2, rotation = 1, handfill = LeftBabyEdge }
    , { fill = 1, rotation = 1, handfill = LeftPalm }
    , { fill = 2, rotation = 9, handfill = LeftThumbEdge }
    , { fill = 3, rotation = 1, handfill = LeftBack }
    ]


handselection : HandSymbol -> Base -> Dict String Size -> Int -> List (Html MainChooser.Types.Msg)
handselection handsymbol base symbolsizes rowheight =
    [ td [ onClick (SelectHand Left), selectedbackground Left handsymbol.hand ]
        [ symbolcentered False base 3 9 symbolsizes 50 rowheight
        , div [] [ text "Left" ]
        ]
    , td [ onClick (SelectHand Right), selectedbackground Right handsymbol.hand ]
        [ symbolcentered False base 3 1 symbolsizes 50 rowheight
        , div [] [ text "Right" ]
        ]
    ]


planeselection : HandSymbol -> Base -> Dict String Size -> Int -> List (Html MainChooser.Types.Msg)
planeselection handsymbol base symbolsizes rowheight =
    [ td [ onClick (SelectPlane Wall), selectedbackground Wall handsymbol.plane ] [ img [ src "./img/wallplane.png", width 70 ] [], div [] [ text "Wall" ] ]
    , td [ onClick (SelectPlane Floor), selectedbackground Floor handsymbol.plane ] [ img [ src "./img/floorplane.png", width 70 ] [], div [] [ text "Floor" ] ]
    ]


selectedbackground : a -> a -> Attribute b
selectedbackground expected currentselected =
    if expected == currentselected then
        style [ "background" => "#7b85c0" ]
    else
        style []


symbolcentered : Bool -> Base -> Fill -> Rotation -> Dict String Size -> Int -> Int -> Html Msg
symbolcentered drag base fill rotation symbolsizes width height =
    let
        symbol =
            getSymbolEditorBaseFillRotation base fill rotation symbolsizes
    in
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
