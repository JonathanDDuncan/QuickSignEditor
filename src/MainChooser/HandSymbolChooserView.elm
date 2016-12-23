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
            getflowersymbols handsymbol
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
                    , "top" => px itemwidth
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
            , div
                [ style
                    [ "position" => "absolute"
                    , "width" => px itemwidth
                    , "height" => px itemheight
                    , "top" => px 0
                    , "left" => px (centered fullwidth itemwidth)
                    , "background" => "blue"
                    ]
                ]
                [ displayflowersymbol flowersymbols.handfill1 base symbolsizes rowheight ]
            , div
                [ style
                    [ "position" => "absolute"
                    , "width" => px itemwidth
                    , "height" => px itemheight
                    , "top" => px (centered (centerfloating * 2) itemwidth)
                    , "left" => px (centered (centerfloating * 2) itemwidth)
                    , "background" => "blue"
                    ]
                ]
                [ displayflowersymbol flowersymbols.handfill2 base symbolsizes rowheight ]
            , div
                [ style
                    [ "position" => "absolute"
                    , "width" => px itemwidth
                    , "height" => px itemheight
                    , "top" => px (centered fullheight itemheight)
                    , "left" => px 0
                    , "background" => "blue"
                    ]
                ]
                [ displayflowersymbol flowersymbols.handfill3 base symbolsizes rowheight ]
            , div
                [ style
                    [ "position" => "absolute"
                    , "width" => px itemwidth
                    , "height" => px itemheight
                    , "bottom" => px (centered (centerfloating * 2) itemwidth)
                    , "left" => px (centered (centerfloating * 2) itemwidth)
                    , "background" => "blue"
                    ]
                ]
                [ displayflowersymbol flowersymbols.handfill4 base symbolsizes rowheight ]
            , div
                [ style
                    [ "position" => "absolute"
                    , "width" => px itemwidth
                    , "height" => px itemheight
                    , "bottom" => px 0
                    , "left" => px (centered fullwidth itemwidth)
                    , "background" => "blue"
                    ]
                ]
                [ displayflowersymbol flowersymbols.handfill5 base symbolsizes rowheight ]
            , div
                [ style
                    [ "position" => "absolute"
                    , "width" => px itemwidth
                    , "height" => px itemheight
                    , "bottom" => px (centered (centerfloating * 2) itemwidth)
                    , "right" => px (centered (centerfloating * 2) itemwidth)
                    , "background" => "blue"
                    ]
                ]
                [ displayflowersymbol flowersymbols.handfill6 base symbolsizes rowheight ]
            , div
                [ style
                    [ "position" => "absolute"
                    , "width" => px itemwidth
                    , "height" => px itemheight
                    , "top" => px (centered fullheight itemheight)
                    , "right" => px 0
                    , "background" => "blue"
                    ]
                ]
                [ displayflowersymbol flowersymbols.handfill7 base symbolsizes rowheight ]
            , div
                [ style
                    [ "position" => "absolute"
                    , "width" => px itemwidth
                    , "height" => px itemheight
                    , "top" => px (centered (centerfloating * 2) itemwidth)
                    , "right" => px (centered (centerfloating * 2) itemwidth)
                    , "background" => "blue"
                    ]
                ]
                [ displayflowersymbol flowersymbols.handfill8 base symbolsizes rowheight ]
            ]


displayflowersymbol handfill base symbolsizes rowheight =
    let
        symbol =
            getSymbolEditorBaseFillRotation base handfill.fill (handfill.rotation + handfill.increment) symbolsizes
    in
        div
            [ style
                [ "width" => px 50
                , "height" => px 50
                , "margin" => "auto"
                ]
                
            ]
            [ div
                [ style
                    [ "position" => "absolute"
                    , "inline" => "block"
                    , "margin" => "auto"
                    , "width" => px 50
                    , "height" => px 50
                    ]
                , onMouseDown (DragSymbol symbol.code)
                ]
                [ generalsymbolcol base handfill.fill (handfill.rotation + handfill.increment) symbolsizes ]
            ]


getflowersymbols handsymbol =
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


rotationincrement handfill increment =
    { fill = handfill.fill, rotation = handfill.rotation, increment = increment - 1, handfill = handfill.handfill }


centered full item =
    toFloat full
        / 2
        - toFloat item
        / 2
        |> truncate


fillsview :
    HandSymbol
    -> Base
    -> Dict String Size
    -> Int
    -> List (Html Msg)
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
                    [ div
                        [ style
                            [ "position" => "relative"
                            , "width" => px 50
                            , "height" => px rowheight
                            ]
                        ]
                        [ div
                            [ style
                                [ "position" => "absolute"
                                , "margin" => "auto"
                                , "width" => px 50
                                , "height" => px rowheight
                                ]
                            ]
                            [ generalsymbolcol base handfill.fill handfill.rotation symbolsizes ]
                        ]
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
        [ div
            [ style
                [ "position" => "relative"
                , "width" => px 50
                , "height" => px rowheight
                ]
            ]
            [ div
                [ style
                    [ "position" => "absolute"
                    , "margin" => "auto"
                    , "width" => px 50
                    , "height" => px rowheight
                    ]
                ]
                [ generalsymbolcol base 3 9 symbolsizes ]
            ]
        , div [] [ text "Left" ]
        ]
    , td [ onClick (SelectHand Right), selectedbackground Right handsymbol.hand ]
        [ div
            [ style
                [ "position" => "relative"
                , "width" => px 50
                , "height" => px rowheight
                ]
            ]
            [ div
                [ style
                    [ "position" => "absolute"
                    , "margin" => "auto"
                    , "width" => px 50
                    , "height" => px rowheight
                    ]
                ]
                [ generalsymbolcol base 3 1 symbolsizes ]
            ]
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


getscales columnwidth rowheight symbols =
    List.minimum (List.map (\symbol -> calcscale symbol.width symbol.height columnwidth rowheight) symbols)


getsymbols base fills rotations symbolsizes =
    let
        symbols =
            List.concatMap (\rotation -> List.map (\fill -> getSymbolEditorBaseFillRotation base fill rotation symbolsizes) fills) rotations
    in
        symbols


generalsymbolonecolumn : Base -> Int -> Int -> List Rotation -> Dict String Size -> Int -> Int -> Float -> List (Html MainChooser.Types.Msg)
generalsymbolonecolumn base symbolcol rotation validrotations symbolsizes columnwidth rowheight scale =
    let
        rotation1 =
            rotation

        rotation2 =
            rotation + 8

        showrotation1 =
            isvalidrotation rotation1 validrotations

        showrotation2 =
            isvalidrotation rotation2 validrotations

        symbol =
            getSymbolEditorBaseFillRotation base symbolcol rotation symbolsizes
    in
        [ if showrotation1 then
            td
                []
                [ div
                    [ style
                        [ "position" => "absolute"
                        , "margin" => "auto"
                        , "width" => "100%"
                        , "height" => "100%"
                        ]
                    
                    ]
                    [ generalsymbolcol base symbolcol rotation symbolsizes ]
                ]
          else
            blanktd
        , blanktd
        , if showrotation2 then
            td
                [ style
                    [ "text-align" => "center"
                    , "display" => "block"
                    , "width" => "45%"
                    ]
                ]
                [ div
                    [ style
                        [ "position" => "absolute"
                        , "margin" => "auto"
                        , "width" => "100%"
                        , "height" => "100%"
                        ]
                  
                    ]
                    [ generalsymbolcol base symbolcol rotation2 symbolsizes ]
                ]
          else
            blanktd
        ]


blanktd : Html a
blanktd =
    td [] []


isvalidrotation : a -> List a -> Bool
isvalidrotation rotation validrotations =
    List.any (\vr -> vr == rotation) validrotations


generalsymbolcol base fill rotation symbolsizes =
    let
        symbol =
            getSymbolEditorBaseFillRotation base fill rotation symbolsizes
    in
        App.map SignView
            (SWEditor.Display.symbolView1 "" symbol)


scaling : number -> List ( String, String )
scaling scale =
    if scale <= 1 then
        [ "transform" => ("scale(" ++ toString scale ++ ")") ]
    else
        []


calcscale : Int -> a -> Int -> Int -> Float
calcscale swidth sheight columnwidth rowheight =
    Basics.min (toFloat columnwidth / toFloat swidth) (toFloat rowheight / toFloat swidth)
