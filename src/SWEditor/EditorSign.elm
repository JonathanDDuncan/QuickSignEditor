module SWEditor.EditorSign exposing (..)

import SWEditor.EditorSymbol exposing (..)
import SW.Types exposing (..)
import SWEditor.Rectangle exposing (..)


type alias EditorSign =
    { width : Int
    , height : Int
    , text : String
    , x : Int
    , y : Int
    , backfill : String
    , syms : List EditorSymbol
    , left : Int
    , lane : Lane
    }


signinit : EditorSign
signinit =
    { width = 0
    , height = 0
    , text = ""
    , x = 0
    , y = 0
    , backfill = ""
    , syms = []
    , left = 0
    , lane = MiddleLane
    }


toEditorSign : Sign -> Int -> EditorSign
toEditorSign sign id =
    let
        editorsymbols =
            List.indexedMap (toEditorSymbol id) sign.syms

        boundingbox =
            getSignBounding editorsymbols

        centeredSmallest =
            centerSignSmallest
                { width = boundingbox.width
                , height = boundingbox.height
                , text = sign.text
                , x = boundingbox.x
                , y = boundingbox.y
                , backfill = sign.backfill
                , syms = editorsymbols
                , left = sign.left
                , lane = MiddleLane
                }
    in
        centeredSmallest


type Lane
    = BLane
    | LeftLane
    | MiddleLane
    | RightLane


centerSignViewposition : NamedPosition -> EditorSign -> EditorSign
centerSignViewposition viewposition sign =
    let
        width =
            viewposition.width

        height =
            viewposition.height

        desiredxcenter =
            width // 2

        desiredycenter =
            height // 2
    in
        centerSign desiredxcenter desiredycenter sign


centerSign : Int -> Int -> EditorSign -> EditorSign
centerSign desiredxcenter desiredycenter sign =
    let
        bounding =
            getSignBounding sign.syms

        currentxcenter =
            bounding.x + bounding.width // 2

        currentycenter =
            bounding.y + bounding.height // 2

        movex =
            desiredxcenter - currentxcenter

        movey =
            desiredycenter - currentycenter

        newsignx =
            sign.x + movex

        newsigny =
            sign.y + movey
    in
        { sign | x = newsignx, y = newsigny, syms = moveSymbols movex movey sign.syms }


centerSignSmallest : EditorSign -> EditorSign
centerSignSmallest sign =
    let
        bounding =
            getSignBounding sign.syms

        desiredxcenter =
            bounding.width // 2

        desiredycenter =
            bounding.height // 2
    in
        centerSign desiredxcenter desiredycenter sign


getSignBounding : List EditorSymbol -> Rect
getSignBounding symbols =
    let
        x1 =
            List.foldr (\s -> min s.x) 10000 symbols

        y1 =
            List.foldr (\s -> min s.y) 10000 symbols

        x2 =
            List.foldr (\s -> max (s.x + round (toFloat s.width * s.size))) 0 symbols

        y2 =
            List.foldr (\s -> max (s.y + round (toFloat s.height * s.size))) 0 symbols
    in
        { x = x1, y = y1, width = x2 - x1, height = y2 - y1 }


getlastuid : EditorSign -> Int
getlastuid editorSign =
    case maximumBy .id editorSign.syms of
        Nothing ->
            0

        Just sign ->
            sign.id


getFsw : EditorSign -> String
getFsw editorSign =
    let
        centered =
            centerSign 500 500 editorSign

        boundingbox =
            "M" ++ toString (centered.x + centered.width) ++ "x" ++ toString (centered.y + centered.height)

        -- M518x533S1870a489x515S18701482x490S20500508x496S2e734500x468
        symbols =
            List.foldr (++) "" (List.map symbolsFsw centered.syms)

        fsw =
            boundingbox ++ symbols
    in
        fsw


symbolsFsw : { c | key : String, x : a, y : b } -> String
symbolsFsw symbol =
    symbol.key ++ toString symbol.x ++ "x" ++ toString symbol.y
