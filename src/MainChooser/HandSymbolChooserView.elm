module MainChooser.HandSymbolChooserView exposing (handsymbolchooser)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ViewHelper.ViewExtra exposing (..)
import MainChooser.Types exposing (..)
import SW.Types exposing (..)
import Dict exposing (..)
import List.Extra exposing (..)
import MainChooser.CompassRoseView exposing (..)


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
                    (fillsview handsymbol rowheight)
                ]
            , compassrose handsymbol 220
            ]


fillsview : HandSymbol -> Int -> List (Html Msg)
fillsview handsymbol rowheight =
    (List.map
        (\( handfillitem, description ) ->
            td
                [ onClick (SelectHandFill handfillitem.filltype)
                , onMouseDown (DragSymbol handfillitem.symbol.code)
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
        (List.Extra.zip
            handsymbol.handfillitems
            [ "Baby Edge", "Palm", "Thumb Edge", "Back" ]
        )
    )


handselection : HandSymbol -> Int -> List (Html Msg)
handselection handsymbol rowheight =
    [ td [ onClick (SelectHand Left), selectedbackground Left handsymbol.hand ]
        [ div
            [ style
                [ "position" => "relative"
                , "display" => "block"
                , "top" => px -8
                , "left" => px -15
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
                , "top" => px -8
                , "left" => px -15
                ]
            ]
            [ symbolcentered False handsymbol.symbolrighthand 50 rowheight ]
        , div [] [ text "Right" ]
        ]
    ]


planeselection : { a | plane : Planes } -> List (Html Msg)
planeselection handsymbol =
    [ td [ onClick (SelectPlane Wall), selectedbackground Wall handsymbol.plane ] [ img [ src "./src/assets/img/wallplanesmall.png", width 50 ] [], div [] [ text "Wall" ] ]
    , td [ onClick (SelectPlane Floor), selectedbackground Floor handsymbol.plane ] [ img [ src "./src/assets/img/floorplanesmall.png", width 50 ] [], div [] [ text "Floor" ] ]
    ]


selectedbackground : a -> a -> Attribute b
selectedbackground expected currentselected =
    if expected == currentselected then
        style [ "background" => "#7b85c0" ]
    else
        style []
