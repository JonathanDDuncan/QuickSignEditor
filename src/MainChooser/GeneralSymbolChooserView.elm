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
        vf =
            getvalidfills choosing.validfills

        vr =
            getvalidrotations choosing.validrotations

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
            Maybe.withDefault 1 <| getscales columnwidth rowheight <| getsymbols choosing.base vf [ 1 ] symbolsizes

        smallestscalebody =
            Maybe.withDefault 1 <| getscales columnwidth rowheight <| (getsymbols choosing.base [ column ] (List.range 1 16) symbolsizes)
    in
        div [ attribute "ondragstart" "return false;", attribute "ondrop" "return false;" ]
            [ table
                [ class "symbolchooserheader"
                , Html.Attributes.style
                    [ "width" => px (width - 12)
                    , "height" => px rowheight
                    ]
                ]
                [ tr [] (generalsymbolrow choosing.base vf 1 symbolsizes columnwidth rowheight smallestscaleheader)
                ]
            , table
                [ Html.Attributes.style
                    [ "width" => px (width - 12)
                    , "height" => px (rowheight * 8)
                    ]
                ]
                [ tr [] (generalsymbolonecolumn choosing.base column 1 vr symbolsizes columnwidth rowheight smallestscalebody)
                , tr [] (generalsymbolonecolumn choosing.base column 2 vr symbolsizes columnwidth rowheight smallestscalebody)
                , tr [] (generalsymbolonecolumn choosing.base column 3 vr symbolsizes columnwidth rowheight smallestscalebody)
                , tr [] (generalsymbolonecolumn choosing.base column 4 vr symbolsizes columnwidth rowheight smallestscalebody)
                , tr [] (generalsymbolonecolumn choosing.base column 5 vr symbolsizes columnwidth rowheight smallestscalebody)
                , tr [] (generalsymbolonecolumn choosing.base column 6 vr symbolsizes columnwidth rowheight smallestscalebody)
                , tr [] (generalsymbolonecolumn choosing.base column 7 vr symbolsizes columnwidth rowheight smallestscalebody)
                , tr [] (generalsymbolonecolumn choosing.base column 8 vr symbolsizes columnwidth rowheight smallestscalebody)
                ]
            ]


getscales columnwidth rowheight symbols =
    List.minimum (List.map (\symbol -> calcscale symbol.width symbol.height columnwidth rowheight) symbols)


getsymbols base fills rotations symbolsizes =
    let
        symbols =
            List.concatMap (\rotation -> List.map (\fill -> getSymbolEditorBaseFillRotation base fill rotation symbolsizes) fills) rotations
    in
        symbols


getvalidfills : String -> List Fill
getvalidfills validfillsstring =
    case validfillsstring of
        "1 - 6" ->
            List.range 1 6

        "1 - 4" ->
            List.range 1 4

        "1, 2" ->
            List.range 1 2

        "1 - 3" ->
            List.range 1 3

        "1 - 5" ->
            List.range 1 5

        "1" ->
            [ 1 ]

        "2" ->
            [ 2 ]

        _ ->
            let
                a =
                    Debug.log "Could not match valid fills string" validfillsstring
            in
                []


getvalidrotations : String -> List Rotation
getvalidrotations validrotationsstring =
    case validrotationsstring of
        "1 - 16" ->
            List.range 1 16

        "1 - 8" ->
            List.range 1 8

        "1" ->
            [ 1 ]

        "1 - 4" ->
            List.range 1 4

        "1, 2, 4, 5, 6, 8" ->
            [ 1, 2, 4, 5, 6, 8 ]

        "1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 14, 16" ->
            [ 1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 14, 16 ]

        "1 - 6" ->
            List.range 1 6

        "1, 2" ->
            List.range 1 2

        "1 - 9" ->
            List.range 1 9

        _ ->
            let
                a =
                    Debug.log "Could not match valid rotations string" validrotationsstring
            in
                []


generalsymbolonecolumn : Base -> Int -> Int -> List Rotation -> Dict String Size -> Int -> Int -> Float -> List (Html MainChooser.Types.Msg)
generalsymbolonecolumn base symbolcol rotation validrotations symbolsizes columnwidth rowheight scale =
    let
        rotation1 =
            rotation

        rotation2 =
            rotation + 8

        showrotation1 =
            isValidRotation rotation1 validrotations

        showrotation2 =
            isValidRotation rotation2 validrotations
    in
        [ if showrotation1 then
            td
                []
                [ (generalsymbolcol True base symbolcol rotation symbolsizes columnwidth rowheight scale) ]
          else
            blanktd
        , blanktd
        , if showrotation2 then
            td
                [ Html.Attributes.style
                    [ "text-align" => "center"
                    , "display" => "block"
                    , "width" => "45%"
                    ]
                ]
                [ generalsymbolcol True base symbolcol rotation2 symbolsizes columnwidth rowheight scale ]
          else
            blanktd
        ]


blanktd : Html a
blanktd =
    td []
        []


isValidRotation : a -> List a -> Bool
isValidRotation rotation validrotations =
    List.any ((==) rotation) validrotations


generalsymbolrow : Base -> List Fill -> Rotation -> Dict String Size -> Int -> Int -> Float -> List (Html MainChooser.Types.Msg)
generalsymbolrow base validfills rotation symbolsizes columnwidth rowheight scale =
    List.map
        (\fill ->
            let
                symbol =
                    getSymbolEditorBaseFillRotation base fill rotation symbolsizes
            in
                td
                    [ onClick (SelectedColumn fill)
                    , onMouseDown (DragSymbol symbol.code)
                    , onDoubleClick (ReplaceSymbol symbol.code)
                    ]
                    [ (generalsymbolcol False base fill rotation symbolsizes columnwidth rowheight scale) ]
        )
        validfills


generalsymbolcol : Bool -> Base -> Fill -> Rotation -> Dict String Size -> Int -> Int -> Float -> Html MainChooser.Types.Msg
generalsymbolcol drag base fill rotation symbolsizes columnwidth rowheight scale =
    let
        symbol =
            getSymbolEditorBaseFillRotation base fill rotation symbolsizes

        sign =
            { syms = [ symbol ]
            }
    in
        -- Html.map SymbolView (symbolView "" symbol)
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
                (signView sign
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


scaling scale =
    if scale <= 1 then
        [ "transform" => ("scale(" ++ toString scale ++ ")") ]
    else
        []


calcscale swidth sheight columnwidth rowheight =
    Basics.min (toFloat columnwidth / toFloat swidth) (toFloat rowheight / toFloat swidth)
