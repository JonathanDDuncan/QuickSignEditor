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
                            , symbol2 : EditorSymbol
                            , symbol1 : EditorSymbol
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
            Maybe.withDefault 1 <| getscales columnwidth rowheight <| List.map (\d -> d.symbol) generalsymbolchooserdata.generalsymbolrowdata

        smallestscalebody =
            Maybe.withDefault 1 <| getscales columnwidth rowheight <| List.map (\d -> d.generalsymbolonecolumndata.symbol1) generalsymbolchooserdata.symbolcolumnsdata
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
                    , symbol1 : EditorSymbol
                    , symbol2 : EditorSymbol
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


getsymbols :
    Base
    -> List Fill
    -> List Int
    -> Dict String Size
    -> List EditorSymbol
getsymbols base fills rotations symbolsizes =
    List.concatMap (\rotation -> List.map (\fill -> getSymbolEditorBaseFillRotation base fill rotation symbolsizes) fills) rotations


generalsymbolonecolumn :
    Float
    -> { b
        | generalsymbolonecolumndata :
            { a
                | show1 : Bool
                , show2 : Bool
                , symbol1 : EditorSymbol
                , symbol2 : EditorSymbol
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
