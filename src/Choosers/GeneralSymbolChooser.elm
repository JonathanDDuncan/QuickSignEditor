module Choosers.GeneralSymbolChooser exposing (getgeneralsymbolchooser, generalsymbolchooser, reorderedcolumnforpetal2)

import Html exposing (Html, div, table, tr, td, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Helpers.ViewExtra exposing (..)
import SWEditor.EditorSymbol exposing (..)
import Choosers.Types exposing (..)
import SWEditor.DisplaySvg exposing (symbolsvgscale)
import SW.Types exposing (..)
import Dict exposing (..)
import Helpers.MaybeExtra exposing (..)
import List.Extra exposing (..)
import Maybe.Extra exposing (..)
import Choosers.CompassRose exposing (..)
import Choosers.Petalhelper exposing (..)


--View


generalsymbolchooser :
    { b | validfills : String }
    -> Int
    -> Int
    -> { d
        | generalsymbolrowdata :
            List { a1 | fill : Int, symbol : Symbol }
        , symbolcolumnsdata :
            SymbolColumnsData
       }
    -> { display : Html Choosers.Types.Msg, width : Int, height : Int }
generalsymbolchooser choosing width height generalsymbolchooserdata =
    let
        vf =
            getvalidfills choosing.validfills

        len =
            toFloat (List.length vf)

        columnwidth =
            truncate <| ((toFloat (width) / 2) / len)

        rowheight =
            30

        smallestscaleheader =
            Maybe.withDefault 1 <|
                getscales columnwidth rowheight <|
                    List.map (\d -> d.symbol) generalsymbolchooserdata.generalsymbolrowdata

        spacerwidth =
            20

        compasswidth =
            (truncate <| toFloat width / 2) - (truncate <| toFloat spacerwidth / 2)

        compassheight =
            compasswidth

        symbolsdisplayer =
            if True then
                showincompassrose generalsymbolchooserdata.symbolcolumnsdata compasswidth compassheight spacerwidth rowheight
            else
                showincolumns generalsymbolchooserdata.symbolcolumnsdata width columnwidth rowheight
    in
        { display =
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
                , symbolsdisplayer.display
                ]
        , width = symbolsdisplayer.width
        , height = symbolsdisplayer.height
        }


showincompassrose : SymbolColumnsData -> Int -> Int -> Int -> Int -> { display : Html Choosers.Types.Msg, width : Int, height : Int }
showincompassrose data fullwidth fullheight spacerwidth rowheight =
    let
        outeritemwidth =
            truncate <| toFloat fullwidth / 3

        outeritemheight =
            outeritemwidth

        innersize =
            1

        reorderedcolumn2 =
            reorderedcolumnforpetal2 data.column2

        petalcontent1 =
            getoutersymbolpetalsMaybe data.column1 outeritemwidth outeritemheight

        petalcontent2 =
            getoutersymbolpetalsMaybe reorderedcolumn2 outeritemwidth outeritemheight

        rosecenter =
            text ""
    in
        { display =
            div
                [ style
                    [ "position" => "relative"
                    , "width" => px (fullwidth * 2 + spacerwidth)
                    , "margin" => "auto"
                    ]
                ]
                [ div
                    [ style
                        [ "position" => "relative"
                        , "float" => "left"
                        ]
                    ]
                    [ compassrosediv fullwidth fullheight outeritemwidth outeritemheight 0 innersize petalcontent1 rosecenter ]
                , div
                    [ style
                        [ "position" => "relative"
                        , "float" => "left"
                        , "width" => px spacerwidth
                        , "height" => px 1
                        ]
                    ]
                    []
                , div
                    [ style
                        [ "position" => "relative"
                        , "float" => "left"
                        ]
                    ]
                    [ compassrosediv fullwidth fullheight outeritemwidth outeritemheight 0 innersize petalcontent2 rosecenter ]
                ]
        , width = fullwidth * 2 + spacerwidth
        , height = rowheight + fullheight + 20
        }



-- Reorder so that is in in rotation order [9,16,15,14,13,12,11,10]


reorderedcolumnforpetal2 : List a -> List a
reorderedcolumnforpetal2 column2data =
    let
        reversedcolumn2 =
            List.reverse column2data

        last =
            List.Extra.last reversedcolumn2 |> Maybe.Extra.toList

        init =
            List.Extra.init reversedcolumn2 |> Maybe.Extra.toList |> List.concat
    in
        List.concat [ last, init ]


getpetalcontent : List (Maybe Symbol) -> Float -> List (Html Msg)
getpetalcontent symbols scale =
    List.map
        (\symbol ->
            case symbol of
                Just symb ->
                    generalsymbolcol True scale symb

                Nothing ->
                    text ""
        )
        symbols


showincolumns : SymbolColumnsData -> Int -> Int -> Int -> { display : Html Choosers.Types.Msg, width : Int, height : Int }
showincolumns data width columnwidth rowheight =
    let
        scale =
            Maybe.withDefault 1 <|
                getscales columnwidth rowheight <|
                    removeNothings <|
                        List.map (\symbol -> symbol) data.column1

        zipped =
            List.Extra.zip data.column1 data.column2
    in
        { display =
            table
                [ Html.Attributes.style
                    [ "width" => px (width - 12)
                    , "height" => px (rowheight * 8)
                    ]
                ]
                (List.map
                    (\data ->
                        row (generalsymbolonerow scale data)
                    )
                    zipped
                )
        , width = width - 12
        , height = rowheight * 10
        }


row : List (Html msg) -> Html msg
row rowdata =
    tr [] (rowdata)


getscales : Int -> Int -> List { a | height : Int, width : Int } -> Maybe Float
getscales columnwidth rowheight symbols =
    List.minimum (List.map (\symbol -> shrinkdontzoom (toFloat symbol.width) (toFloat symbol.height) (toFloat columnwidth) (toFloat rowheight)) symbols)


generalsymbolonerow :
    Float
    -> ( Maybe Symbol, Maybe Symbol )
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


showrotation : Maybe Symbol -> Float -> Html Msg
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
                Symbol
        }
    -> Float
    -> List (Html Msg)
generalsymbolrow generalsymbolrowdata scale =
    List.map
        (\d ->
            td
                [ onClick ((Choosers.Types.Editor << SelectedColumn) d.fill)
                , onMouseDown ((Editor << DragSymbol) d.symbol.key)
                , onDoubleClick ((Editor << ReplaceSymbol) d.symbol.key)
                ]
                [ (generalsymbolcol False scale d.symbol) ]
        )
        generalsymbolrowdata


generalsymbolcol : Bool -> Float -> Symbol -> Html Msg
generalsymbolcol drag scale symbol =
    div
        [ onMouseDown
            (if (drag) then
                (Editor << DragSymbol) symbol.key
             else
                Noop
            )
        , onDoubleClick
            ((Editor << ReplaceSymbol) symbol.key)
        ]
        [ Html.map SignView
            (symbolsvgscale scale "hover" symbol)
        ]



--State


getgeneralsymbolchooser :
    { a | base : Base, validfills : String, validrotations : String }
    -> Dict String Size
    -> Int
    -> { generalsymbolrowdata : List { fill : Int, symbol : Symbol }
       , symbolcolumnsdata :
            SymbolColumnsData
       }
getgeneralsymbolchooser choosing symbolsizes selectedcolumn =
    let
        vf =
            getvalidfills choosing.validfills

        vr =
            getvalidrotations choosing.validrotations

        column =
            if isValidRotation selectedcolumn vf then
                selectedcolumn
            else
                1

        generalsymbolrowdata =
            getsymbolfill choosing.base 1 vf symbolsizes

        symbolcolumnsdata =
            getsymbolcolumnsdata choosing.base column vr symbolsizes
    in
        { generalsymbolrowdata = generalsymbolrowdata, symbolcolumnsdata = symbolcolumnsdata }


type alias SymbolColumnsData =
    { column1 : List (Maybe Symbol)
    , column2 : List (Maybe Symbol)
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


getsymbol : Base -> Fill -> Int -> List Rotation -> Dict String Size -> Maybe Symbol
getsymbol base fill rotation validrotations symbolsizes =
    let
        isvalid =
            isValidRotation rotation validrotations

        symbol =
            if isvalid then
                Just <| getSymbolbyBaseFillRotation base fill rotation symbolsizes
            else
                Nothing
    in
        symbol


getsymbolfill :
    Base
    -> Rotation
    -> List Fill
    -> Dict String Size
    -> List { fill : Int, symbol : Symbol }
getsymbolfill base rotation validfills symbolsizes =
    List.map
        (\fill ->
            let
                symbol =
                    getSymbolbyBaseFillRotation base fill rotation symbolsizes
            in
                { symbol = symbol, fill = fill }
        )
        validfills
