module SWEditor.EditorSign
    exposing
        ( updateSymbolIds
        , centerSign
        , centerSignViewposition
        , colorallsymbols
        , colorsymbols
        , sizesymbols
        , adjustpositionsymbols
        , getSignBounding
        , getlastuid
        , getlastsignuid
        )

import SWEditor.EditorSymbol exposing (updateIds, moveSymbols, colorsymbol, sizesymbol, adjustposition)
import SW.Types exposing (Symbol, Sign, Colors, NamedPosition, maximumBy)
import SWEditor.Rectangle exposing (..)
import Helpers.ResultExtra exposing (..)


updateSymbolIds : Sign -> Int -> Sign
updateSymbolIds sign id =
    let
        editorsymbols =
            updateIds id sign.syms

        boundingbox =
            getSignBounding editorsymbols

        centeredSmallest =
            centerSignSmallest
                { sign
                    | width = boundingbox.width
                    , height = boundingbox.height
                    , x = boundingbox.x
                    , y = boundingbox.y
                    , syms = editorsymbols
                }
    in
        centeredSmallest


centerSignViewposition : NamedPosition -> Sign -> Sign
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


centerSign : Int -> Int -> Sign -> Sign
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

        movedsymbols =
            moveSymbols movex movey sign.syms

        newbounding =
            getSignBounding movedsymbols
    in
        { sign | width = newbounding.width, height = newbounding.height, x = newbounding.x, y = newbounding.y, syms = movedsymbols }


centerSignSmallest : Sign -> Sign
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


getSignBounding : List Symbol -> Rect
getSignBounding symbols =
    let
        maxvalue =
            if List.length symbols == 0 then
                0
            else
                10000

        x1 =
            List.foldr (\s -> min s.x) maxvalue symbols

        y1 =
            List.foldr (\s -> min s.y) maxvalue symbols

        x2 =
            List.foldr (\s -> max (s.x + round (toFloat s.width * s.size))) 0 symbols

        y2 =
            List.foldr (\s -> max (s.y + round (toFloat s.height * s.size))) 0 symbols
    in
        { x = x1, y = y1, width = x2 - x1, height = y2 - y1 }


getlastsignuid : Sign -> Int
getlastsignuid editorSign =
    getlastuid editorSign.syms


getlastuid : List Symbol -> Int
getlastuid symbols =
    case maximumBy .id symbols of
        Nothing ->
            0

        Just sign ->
            sign.id


colorallsymbols : Result String Colors -> Sign -> Result String Sign
colorallsymbols colors sign =
    colors
        |> andThentoResult (colorallsymbols1 sign)


colorallsymbols1 : Sign -> Colors -> Sign
colorallsymbols1 sign colors =
    if colors.nbcolor == Nothing && colors.nwcolor == Nothing then
        sign
    else
        { sign | syms = List.map (\syms -> colorsymbol colors syms) sign.syms }


colorsymbols : Result String (List { colors : Colors, pos : Int }) -> Result String Sign -> Result String Sign
colorsymbols symbolscolors sign =
    symbolscolors
        |> Result.andThen
            (\symbolscolors1 ->
                sign
                    |> andThentoResult
                        (colorsymbols1 symbolscolors1)
            )


colorsymbols1 : List { colors : Colors, pos : Int } -> Sign -> Sign
colorsymbols1 symbolscolors sign =
    { sign
        | syms =
            List.indexedMap
                (\index symbol ->
                    applychangetoSymbol (\symbolcolor -> colorsymbol symbolcolor.colors symbol) symbol (index + 1) symbolscolors
                )
                sign.syms
    }


sizesymbols : Result String (List { size : Float, pos : Int, adjustment : { x : Int, y : Int } }) -> Result String Sign -> Result String Sign
sizesymbols symbolsizes sign =
    symbolsizes
        |> Result.andThen
            (\symbolsizes1 ->
                sign
                    |> andThentoResult
                        (sizesymbols1 symbolsizes1)
            )


sizesymbols1 : List { size : Float, pos : Int, adjustment : { x : Int, y : Int } } -> Sign -> Sign
sizesymbols1 symbolsizes sign =
    { sign
        | syms =
            List.indexedMap
                (\index symbol ->
                    applychangetoSymbol (\symbolsize -> sizesymbol symbolsize symbol) symbol (index + 1) symbolsizes
                )
                sign.syms
    }


adjustpositionsymbols :
    Result String (List { size : Float, pos : Int, adjustment : { x : Int, y : Int } })
    -> Result String Sign
    -> Result String Sign
adjustpositionsymbols symbolsizes sign =
    symbolsizes
        |> Result.andThen
            (\symbolsizes1 ->
                sign
                    |> andThentoResult
                        (adjustpositionsymbols1 symbolsizes1)
            )


adjustpositionsymbols1 : List { size : Float, pos : Int, adjustment : { x : Int, y : Int } } -> Sign -> Sign
adjustpositionsymbols1 symbolsizes sign =
    { sign
        | syms =
            List.indexedMap
                (\index symbol ->
                    applychangetoSymbol (\symbolsize -> adjustposition symbolsize symbol) symbol (index + 1) symbolsizes
                )
                sign.syms
    }


applychangetoSymbol : ({ b | pos : Int } -> Symbol) -> Symbol -> Int -> List { b | pos : Int } -> Symbol
applychangetoSymbol action symbol index changedata =
    changedata
        |> List.filter (\item -> item.pos == index)
        |> List.map action
        |> List.head
        |> Maybe.withDefault symbol
