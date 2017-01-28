module MainChooser.GeneralSymbolChooserView exposing (generalsymbolchooser)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.EditorSymbol exposing (..)
import MainChooser.Types exposing (..)
import SWEditor.Display exposing (signView)
import SW.Types exposing (..)
import Dict exposing (..)


generalsymbolchooser : ChooserItem -> Fill -> Dict String Size -> Int -> Int -> Html Msg
generalsymbolchooser choosing selectedcolumn symbolsizes width height =
    let
        data =
            getgeneralsymbolchooser choosing.validfills choosing.validrotations symbolsizes selectedcolumn choosing.base width height
    in
        div [ attribute "ondragstart" "return false;", attribute "ondrop" "return false;" ]
            [ table
                [ class "symbolchooserheader"
                , Html.Attributes.style
                    [ "width" => px (width - 12)
                    , "height" => px data.rowheight
                    ]
                ]
                [ tr [] (generalsymbolrow data.generalsymbolrowdata data.scale)
                ]
            , table
                [ Html.Attributes.style
                    [ "width" => px (width - 12)
                    , "height" => px (data.rowheight * 8)
                    ]
                ]
                (symbolcolumns data.symbolcolumnsdata)
            ]


getgeneralsymbolchooser :
    String
    -> String
    -> Dict String Size
    -> Int
    -> Base
    -> Int
    -> Int
    -> { generalsymbolrowdata : List { fill : Int, symbol : EditorSymbol }
       , rowheight : Int
       , scale : Float
       , symbolcolumnsdata :
            List
                { generalsymbolonecolumndata :
                    { show1 : Bool
                    , show2 : Bool
                    , symbol1 : EditorSymbol
                    , symbol2 : EditorSymbol
                    }
                , scale : Float
                }
       }
getgeneralsymbolchooser validfills validrotations symbolsizes selectedcolumn base width height =
    let
        vf =
            getvalidfills validfills

        vr =
            getvalidrotations validrotations

        column =
            if isValidRotation selectedcolumn vf then
                selectedcolumn
            else
                1

        rowheight =
            truncate <| toFloat height / toFloat 10

        len =
            toFloat (List.length vf)

        columnwidth =
            truncate <| ((toFloat (width) / 2) / len)

        smallestscaleheader =
            Maybe.withDefault 1 <| getscales columnwidth rowheight <| getsymbols base vf [ 1 ] symbolsizes

        smallestscalebody =
            Maybe.withDefault 1 <| getscales columnwidth rowheight <| (getsymbols base [ column ] (List.range 1 16) symbolsizes)

        generalsymbolrowdata =
            getsymbolfill base 1 vf symbolsizes

        symbolcolumnsdata =
            getsymbolcolumnsdata base column vr symbolsizes smallestscalebody
    in
        { rowheight = rowheight, scale = smallestscaleheader, generalsymbolrowdata = generalsymbolrowdata, symbolcolumnsdata = symbolcolumnsdata }


symbolcolumns :
    List
        { b
            | generalsymbolonecolumndata :
                { a
                    | show1 : Bool
                    , show2 : Bool
                    , symbol1 : EditorSymbol
                    , symbol2 : EditorSymbol
                }
            , scale : comparable
        }
    -> List (Html Msg)
symbolcolumns symbolcolumnsdata =
    List.map row (List.map generalsymbolonecolumn symbolcolumnsdata)


getsymbolcolumnsdata :
    Base
    -> Fill
    -> List Rotation
    -> Dict String Size
    -> a
    -> List
        { generalsymbolonecolumndata :
            { show1 : Bool
            , show2 : Bool
            , symbol1 : EditorSymbol
            , symbol2 : EditorSymbol
            }
        , scale : a
        }
getsymbolcolumnsdata base column vr symbolsizes smallestscalebody =
    List.map
        (\rotation ->
            getrowdata base column rotation vr symbolsizes smallestscalebody
        )
        (List.range 1 8)


getrowdata :
    Base
    -> Fill
    -> Int
    -> List Rotation
    -> Dict String Size
    -> a
    -> { generalsymbolonecolumndata :
            { show1 : Bool
            , show2 : Bool
            , symbol1 : EditorSymbol
            , symbol2 : EditorSymbol
            }
       , scale : a
       }
getrowdata base column rotation vr symbolsizes scale =
    let
        generalsymbolonecolumndata =
            getgeneralsymbolonecolumndata base column rotation vr symbolsizes
    in
        { generalsymbolonecolumndata = generalsymbolonecolumndata, scale = scale }


row : List (Html msg) -> Html msg
row rowdata =
    tr [] (rowdata)


getscales : Int -> Int -> List { a | height : Int, width : Int } -> Maybe Float
getscales columnwidth rowheight symbols =
    List.minimum (List.map (\symbol -> calcscale symbol.width symbol.height columnwidth rowheight) symbols)


getsymbols :
    Base
    -> List Fill
    -> List Int
    -> Dict String Size
    -> List EditorSymbol
getsymbols base fills rotations symbolsizes =
    List.concatMap (\rotation -> List.map (\fill -> getSymbolEditorBaseFillRotation base fill rotation symbolsizes) fills) rotations


generalsymbolonecolumn :
    { b
        | generalsymbolonecolumndata :
            { a
                | show1 : Bool
                , show2 : Bool
                , symbol1 : EditorSymbol
                , symbol2 : EditorSymbol
            }
        , scale : comparable
    }
    -> List (Html Msg)
generalsymbolonecolumn data =
    [ blanktd
    , blanktd
    , showrotation data.generalsymbolonecolumndata.symbol1 data.generalsymbolonecolumndata.show1 data.scale
    , blanktd
    , showrotation data.generalsymbolonecolumndata.symbol2 data.generalsymbolonecolumndata.show2 data.scale
    ]


getgeneralsymbolonecolumndata :
    Base
    -> Fill
    -> Int
    -> List Rotation
    -> Dict String Size
    -> { show1 : Bool
       , symbol1 : EditorSymbol
       , symbol2 : EditorSymbol
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
            getSymbolEditorBaseFillRotation base fill rotation1 symbolsizes

        symbol2 =
            getSymbolEditorBaseFillRotation base fill rotation2 symbolsizes
    in
        { show1 = showrotation1, symbol1 = symbol1, show2 = showrotation2, symbol2 = symbol2 }


showrotation : EditorSymbol -> Bool -> comparable -> Html Msg
showrotation symbol show scale =
    if show then
        td
            [ Html.Attributes.style
                [ "text-align" => "center"
                , "display" => "block"
                , "width" => "45%"
                ]
            ]
            [ generalsymbolcol True scale symbol ]
    else
        blanktd


blanktd : Html a
blanktd =
    td []
        []


isValidRotation : Int -> List Int -> Bool
isValidRotation rotation validrotations =
    List.any ((==) rotation) validrotations


generalsymbolrow :
    List
        { a
            | fill : Int
            , symbol :
                EditorSymbol
        }
    -> comparable
    -> List (Html Msg)
generalsymbolrow generalsymbolrowdata scale =
    List.map
        (\d ->
            td
                [ onClick (SelectedColumn d.fill)
                , onMouseDown (DragSymbol d.symbol.code)
                , onDoubleClick (ReplaceSymbol d.symbol.code)
                ]
                [ (generalsymbolcol False scale d.symbol) ]
        )
        generalsymbolrowdata


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


generalsymbolcol : Bool -> comparable -> EditorSymbol -> Html Msg
generalsymbolcol drag scale symbol =
    div
        [ onMouseDown
            (if (drag) then
                DragSymbol symbol.code
             else
                Noop
            )
        , onDoubleClick
            (ReplaceSymbol symbol.code)
        , Html.Attributes.style (scaling scale)
        ]
        [ Html.map SignView
            (signView { syms = [ symbol ] }
                [ Html.Attributes.style
                    [ "position" => "relative"
                    , "margin" => "auto"
                    , "left" => px 0
                    , "top" => px 4
                    , "width" => px symbol.width
                    , "height" => px symbol.height
                    ]
                ]
            )
        ]


scaling : comparable -> List ( String, String )
scaling scale =
    if scale <= 1 then
        [ "transform" => ("scale(" ++ toString scale ++ ")") ]
    else
        []


calcscale : Int -> Int -> Int -> Int -> Float
calcscale swidth sheight columnwidth rowheight =
    Basics.min (toFloat columnwidth / toFloat swidth) (toFloat rowheight / toFloat sheight)
