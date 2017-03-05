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
import List.Extra exposing (..)


--View


generalsymbolchooser :
    { b | validfills : String }
    -> Int
    -> Int
    -> { d
        | generalsymbolrowdata :
            List { a1 | fill : Int, symbol : EditorSymbol }
        , symbolcolumnsdata :
            SymbolColumnsData
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
                    removeNothings <|
                        List.map (\symbol -> symbol) generalsymbolchooserdata.symbolcolumnsdata.column1
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
                (showincolumns generalsymbolchooserdata.symbolcolumnsdata smallestscalebody)
            ]


showincolumns :
    SymbolColumnsData
    -> Float
    -> List (Html Msg)
showincolumns data scale =
    let
        zipped =
            List.Extra.zip data.column1 data.column2
    in
        List.map
            (\data ->
                row (generalsymbolonerow scale data)
            )
            zipped


row : List (Html msg) -> Html msg
row rowdata =
    tr [] (rowdata)


getscales : Int -> Int -> List { a | height : Int, width : Int } -> Maybe Float
getscales columnwidth rowheight symbols =
    List.minimum (List.map (\symbol -> calcscale symbol.width symbol.height columnwidth rowheight) symbols)


generalsymbolonerow :
    Float
    -> ( Maybe EditorSymbol, Maybe EditorSymbol )
    -> List (Html Msg)
generalsymbolonerow scale data =
    let
        symbol1 =
            Tuple.first data

        symbol2 =
            Tuple.second data
    in
        [ blanktd
        , blanktd
        , showrotation symbol1 scale
        , blanktd
        , showrotation symbol2 scale
        ]


showrotation : Maybe EditorSymbol -> Float -> Html Msg
showrotation symbol scale =
    case symbol of
        Just symb ->
            td
                [ Html.Attributes.style
                    [ "text-align" => "center"
                    , "display" => "block"
                    , "width" => "45%"
                    ]
                ]
                [ generalsymbolcol True scale symb ]

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
            SymbolColumnsData
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


type alias SymbolColumnsData =
    { column1 : List (Maybe EditorSymbol)
    , column2 : List (Maybe EditorSymbol)
    }


getsymbolcolumnsdata :
    Base
    -> Fill
    -> List Rotation
    -> Dict String Size
    -> SymbolColumnsData
getsymbolcolumnsdata base column vr symbolsizes =
    let
        column1 =
            List.map
                (\rotation ->
                    getsymbol base column rotation vr symbolsizes
                )
                (List.range 1 8)

        column2 =
            List.map
                (\rotation ->
                    getsymbol base column rotation vr symbolsizes
                )
                (List.range 9 16)
    in
        { column1 = column1, column2 = column2 }


getsymbol : Base -> Fill -> Int -> List Rotation -> Dict String Size -> Maybe EditorSymbol
getsymbol base fill rotation validrotations symbolsizes =
    let
        isvalid =
            isValidRotation rotation validrotations

        symbol =
            if isvalid then
                Just <| getSymbolEditorBaseFillRotation base fill rotation symbolsizes
            else
                Nothing
    in
        symbol


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
