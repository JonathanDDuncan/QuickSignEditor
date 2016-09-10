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
    , laned : Bool
    , left : Int
    }


toEditorSign : Sign -> Int -> EditorSign
toEditorSign sign id =
    { width = sign.width
    , height = sign.height
    , text = sign.text
    , x = sign.x
    , y = sign.y
    , backfill = sign.backfill
    , syms = List.indexedMap (toEditorSymbol id) sign.syms
    , laned = sign.laned
    , left = sign.left
    }


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
            getSignBounding sign

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
            getSignBounding sign

        desiredxcenter =
            bounding.width // 2

        desiredycenter =
            bounding.height // 2
    in
        centerSign desiredxcenter desiredycenter sign


getSignBounding : EditorSign -> Rect
getSignBounding sign =
    let
        x1 =
            List.foldr (\s -> min s.x) 10000 sign.syms

        y1 =
            List.foldr (\s -> min s.y) 10000 sign.syms

        x2 =
            List.foldr (\s -> max (s.x + s.width)) 0 sign.syms

        y2 =
            List.foldr (\s -> max (s.y + s.height)) 0 sign.syms
    in
        { x = x1, y = y1, width = x2 - x1, height = y2 - y1 }


getlastuid : EditorSign -> Int
getlastuid editorSign =
    case maximumBy .id editorSign.syms of
        Nothing ->
            0

        Just sign ->
            sign.id
