module Choosers.GeneralSymbolChooser exposing (getgeneralsymbolchooser, generalsymbolchooser)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.EditorSymbol exposing (..)
import Choosers.Types exposing (..)
import SWEditor.DisplaySvg exposing (symbolsvgscale)
import SW.Types exposing (..)
import Dict exposing (..)
import MaybeHelper.MaybeExtra exposing (..)


--View


generalsymbolchooser :
    { b | validfills : String }
    -> Int
    -> Int
    -> { d
        | generalsymbolrowdata :
            List { a1 | fill : Int, symbol : EditorSymbol }
        , symbolcolumnsdata :
            List
                { c
                    | generalsymbolonecolumndata :
                        { a
                            | show1 : Bool
                            , show2 : Bool
                            , symbol2 : Maybe EditorSymbol
                            , symbol1 : Maybe EditorSymbol
                        }
                }
       }
    -> Html Msg
generalsymbolchooser choosing width height generalsymbolchooserdata =
    let
        vf =
            getvalidfills choosing.validfills

        len =
            toFloat (List.length vf)

        columnwidth =
            truncate <| ((toFloat (width) / 2) / len)

        rowheight =
            truncate <| toFloat height / toFloat 10

        smallestscaleheader =
            Maybe.withDefault 1 <|
                getscales columnwidth rowheight <|
                    List.map (\d -> d.symbol) generalsymbolchooserdata.generalsymbolrowdata

        smallestscalebody =
            Maybe.withDefault 1 <|
                getscales columnwidth rowheight <|
                    List.filterMap (\value -> value) <|
                        List.map (\d -> d.generalsymbolonecolumndata.symbol1) generalsymbolchooserdata.symbolcolumnsdata
    in
        div [ attribute "ondragstart" "return false;", attribute "ondrop" "return false;" ]
            [ table
                [ class "symbolchooserheader"
                , Html.Attributes.style
                    [ "width" => px (width - 12)
                    , "height" => px rowheight
                    ]
                ]
                [ tr [] (generalsymbolrow generalsymbolchooserdata.generalsymbolrowdata smallestscaleheader)
                ]
            , table
                [ Html.Attributes.style
                    [ "width" => px (width - 12)
                    , "height" => px (rowheight * 8)
                    ]
                ]
                (symbolcolumns generalsymbolchooserdata.symbolcolumnsdata smallestscalebody)
            ]


symbolcolumns :
    List
        { b
            | generalsymbolonecolumndata :
                { a
                    | show1 : Bool
                    , show2 : Bool
                    , symbol1 : Maybe EditorSymbol
                    , symbol2 : Maybe EditorSymbol
                }
        }
    -> Float
    -> List (Html Msg)
symbolcolumns symbolcolumnsdata scale =
    List.map row (List.map (generalsymbolonecolumn scale) symbolcolumnsdata)


row : List (Html msg) -> Html msg
row rowdata =
    tr [] (rowdata)


getscales : Int -> Int -> List { a | height : Int, width : Int } -> Maybe Float
getscales columnwidth rowheight symbols =
    List.minimum (List.map (\symbol -> calcscale symbol.width symbol.height columnwidth rowheight) symbols)


generalsymbolonecolumn :
    Float
    -> { b
        | generalsymbolonecolumndata :
            { a
                | show1 : Bool
                , show2 : Bool
                , symbol1 : Maybe EditorSymbol
                , symbol2 : Maybe EditorSymbol
            }
       }
    -> List (Html Msg)
generalsymbolonecolumn scale data =
    [ blanktd
    , blanktd
    , showrotation data.generalsymbolonecolumndata.symbol1 data.generalsymbolonecolumndata.show1 scale
    , blanktd
    , showrotation data.generalsymbolonecolumndata.symbol2 data.generalsymbolonecolumndata.show2 scale
    ]


showrotation : Maybe EditorSymbol -> Bool -> Float -> Html Msg
showrotation symbol show scale =
    case symbol of
        Just symb ->
            if show then
                td
                    [ Html.Attributes.style
                        [ "text-align" => "center"
                        , "display" => "block"
                        , "width" => "45%"
                        ]
                    ]
                    [ generalsymbolcol True scale symb ]
            else
                blanktd

        Nothing ->
            blanktd


blanktd : Html a
blanktd =
    td []
        []


generalsymbolrow :
    List
        { a
            | fill : Int
            , symbol :
                EditorSymbol
        }
    -> Float
    -> List (Html Msg)
generalsymbolrow generalsymbolrowdata scale =
    List.map
        (\d ->
            td
                [ onClick (SelectedColumn d.fill)
                , onMouseDown (DragSymbol d.symbol.key)
                , onDoubleClick (ReplaceSymbol d.symbol.key)
                ]
                [ (generalsymbolcol False scale d.symbol) ]
        )
        generalsymbolrowdata


generalsymbolcol : Bool -> Float -> EditorSymbol -> Html Msg
generalsymbolcol drag scale symbol =
    div
        [ onMouseDown
            (if (drag) then
                DragSymbol symbol.key
             else
                Noop
            )
        , onDoubleClick
            (ReplaceSymbol symbol.key)
        ]
        [ Html.map SignView
            (symbolsvgscale scale symbol)
        ]


calcscale : Int -> Int -> Int -> Int -> Float
calcscale swidth sheight columnwidth rowheight =
    Basics.min (toFloat columnwidth / toFloat swidth) (toFloat rowheight / toFloat sheight)



--State


getgeneralsymbolchooser :
    { a | base : Base, validfills : String, validrotations : String }
    -> Dict String Size
    -> Int
    -> { generalsymbolrowdata : List { fill : Int, symbol : EditorSymbol }
       , symbolcolumnsdata :
            List
                { generalsymbolonecolumndata :
                    { show1 : Bool
                    , show2 : Bool
                    , symbol1 : Maybe EditorSymbol
                    , symbol2 : Maybe EditorSymbol
                    }
                }
       }
getgeneralsymbolchooser choosing symbolsizes selectedcolumn =
    let
        validfills =
            choosing.validfills

        validrotations =
            choosing.validrotations

        base =
            choosing.base

        vf =
            getvalidfills validfills

        vr =
            getvalidrotations validrotations

        column =
            if isValidRotation selectedcolumn vf then
                selectedcolumn
            else
                1

        generalsymbolrowdata =
            getsymbolfill base 1 vf symbolsizes

        symbolcolumnsdata =
            getsymbolcolumnsdata base column vr symbolsizes
    in
        { generalsymbolrowdata = generalsymbolrowdata, symbolcolumnsdata = symbolcolumnsdata }


getsymbolcolumnsdata :
    Base
    -> Fill
    -> List Rotation
    -> Dict String Size
    -> List
        { generalsymbolonecolumndata :
            { show1 : Bool
            , show2 : Bool
            , symbol1 : Maybe EditorSymbol
            , symbol2 : Maybe EditorSymbol
            }
        }
getsymbolcolumnsdata base column vr symbolsizes =
    List.map
        (\rotation ->
            getrowdata base column rotation vr symbolsizes
        )
        (List.range 1 8)


getrowdata :
    Base
    -> Fill
    -> Int
    -> List Rotation
    -> Dict String Size
    -> { generalsymbolonecolumndata :
            { show1 : Bool
            , show2 : Bool
            , symbol1 : Maybe EditorSymbol
            , symbol2 : Maybe EditorSymbol
            }
       }
getrowdata base column rotation vr symbolsizes =
    let
        generalsymbolonecolumndata =
            getgeneralsymbolonecolumndata base column rotation vr symbolsizes
    in
        { generalsymbolonecolumndata = generalsymbolonecolumndata }


getgeneralsymbolonecolumndata :
    Base
    -> Fill
    -> Int
    -> List Rotation
    -> Dict String Size
    -> { show1 : Bool
       , symbol1 : Maybe EditorSymbol
       , symbol2 : Maybe EditorSymbol
       , show2 : Bool
       }
getgeneralsymbolonecolumndata base fill rotation validrotations symbolsizes =
    let
        rotation1 =
            rotation

        rotation2 =
            rotation + 8

        showrotation1 =
            isValidRotation rotation1 validrotations

        showrotation2 =
            isValidRotation rotation2 validrotations

        symbol1 =
            if showrotation1 then
                Just <| getSymbolEditorBaseFillRotation base fill rotation1 symbolsizes
            else
                Nothing

        symbol2 =
            if showrotation2 then
                Just <| getSymbolEditorBaseFillRotation base fill rotation2 symbolsizes
            else
                Nothing
    in
        { show1 = showrotation1, symbol1 = symbol1, show2 = showrotation2, symbol2 = symbol2 }


getsymbolfill :
    Base
    -> Rotation
    -> List Fill
    -> Dict String Size
    -> List { fill : Int, symbol : EditorSymbol }
getsymbolfill base rotation validfills symbolsizes =
    List.map
        (\fill ->
            let
                symbol =
                    getSymbolEditorBaseFillRotation base fill rotation symbolsizes
            in
                { symbol = symbol, fill = fill }
        )
        validfills
