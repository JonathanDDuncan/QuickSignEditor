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
import SWEditor.EditorSymbol exposing (..)


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
            , compassrose handsymbol 220
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
