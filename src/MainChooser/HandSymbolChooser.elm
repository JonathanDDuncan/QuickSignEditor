module MainChooser.HandSymbolChooser exposing (..)

import MainChooser.Types exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SW.Types exposing (..)
import Dict exposing (..)
import MainChooser.HandPng exposing (..)


--View

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ViewHelper.ViewExtra exposing (..)
import MainChooser.Types exposing (..)
import List.Extra exposing (..)
import SWEditor.EditorSymbol exposing (..)
import MainChooser.CompassRose exposing (..)


handsymbolchooser handsymbol rosepetaldata width height =
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
            , compassrose handsymbol.handfill rosepetaldata 220
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
                        , "top" => px -8
                        , "left" => px -15
                        ]
                    ]
                    [ symbolcentered False handfillitem.symbol 50 rowheight
                    ]
                , div [] [ text description ]
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
    [ handselection handsymbol rowheight Left .symbollefthand "Left"
    , handselection handsymbol rowheight Right .symbolrighthand "Right"
    ]


handselection :
    { a | hand : Hands }
    -> Int
    -> Hands
    -> ({ a | hand : Hands } -> EditorSymbol)
    -> String
    -> Html Msg
handselection handsymbol rowheight handType symbolgetter label =
    td [ onClick (SelectHand handType), selectedbackground handType handsymbol.hand ]
        [ div
            [ style
                [ "position" => "relative"
                , "display" => "block"
                , "top" => px -8
                , "left" => px -15
                ]
            ]
            [ symbolcentered False (symbolgetter handsymbol) 50 rowheight ]
        , div [] [ text label ]
        ]


pngfolder : String
pngfolder =
    "./assets/img/"


planeselection : { a | plane : Planes } -> List (Html Msg)
planeselection handsymbol =
    [ td
        [ onClick (SelectPlane Wall), selectedbackground Wall handsymbol.plane ]
        [ img [ src <| pngfolder ++ "wallplanesmall.png", width 50 ] []
        , div [] [ text "Wall" ]
        ]
    , td
        [ onClick (SelectPlane Floor), selectedbackground Floor handsymbol.plane ]
        [ img [ src <| pngfolder ++ "floorplanesmall.png", width 50 ] []
        , div [] [ text "Floor" ]
        ]
    ]


selectedbackground : a -> a -> Attribute b
selectedbackground expected currentselected =
    if expected == currentselected then
        style [ "background" => "#7b85c0" ]
    else
        style []



--State


gethandfillitems : Base -> Dict.Dict String Size -> Hands -> Planes -> List HandFillItem
gethandfillitems base symbolsizes handtype planetype =
    if handtype == Right then
        [ gethandfillitem base symbolsizes RightBabyEdge planetype
        , gethandfillitem base symbolsizes RightPalm planetype
        , gethandfillitem base symbolsizes RightThumbEdge planetype
        , gethandfillitem base symbolsizes RightBack planetype
        ]
    else
        [ gethandfillitem base symbolsizes LeftBabyEdge planetype
        , gethandfillitem base symbolsizes LeftPalm planetype
        , gethandfillitem base symbolsizes LeftThumbEdge planetype
        , gethandfillitem base symbolsizes LeftBack planetype
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
    , createhandfillitem base symbolsizes { fill = 1, rotation = 9, filltype = LeftPalm, planetype = Wall }
    , createhandfillitem base symbolsizes { fill = 2, rotation = 9, filltype = LeftThumbEdge, planetype = Wall }
    , createhandfillitem base symbolsizes { fill = 3, rotation = 9, filltype = LeftBack, planetype = Wall }
    , createhandfillitem base symbolsizes { fill = 5, rotation = 9, filltype = RightBabyEdge, planetype = Floor }
    , createhandfillitem base symbolsizes { fill = 4, rotation = 1, filltype = RightPalm, planetype = Floor }
    , createhandfillitem base symbolsizes { fill = 5, rotation = 1, filltype = RightThumbEdge, planetype = Floor }
    , createhandfillitem base symbolsizes { fill = 6, rotation = 1, filltype = RightBack, planetype = Floor }
    , createhandfillitem base symbolsizes { fill = 5, rotation = 1, filltype = LeftBabyEdge, planetype = Floor }
    , createhandfillitem base symbolsizes { fill = 4, rotation = 9, filltype = LeftPalm, planetype = Floor }
    , createhandfillitem base symbolsizes { fill = 5, rotation = 9, filltype = LeftThumbEdge, planetype = Floor }
    , createhandfillitem base symbolsizes { fill = 6, rotation = 9, filltype = LeftBack, planetype = Floor }
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
