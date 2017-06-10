module SWEditor.EditorSign
    exposing
        ( colorallsymbols
        , colorsymbols
        , sizesymbols
        , adjustpositionsymbols
        )

import SWEditor.EditorSymbol exposing (colorsymbol, sizesymbol, adjustposition)
import SW.Types exposing (Colors, NamedPosition)
import SW.Sign exposing (Sign)
import SW.Symbol exposing (Symbol)
import Helpers.ResultExtra exposing (andThentoResult)


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
    let
        syms =
            List.indexedMap
                (\index symbol ->
                    applychangetoSymbol (\symbolcolor -> colorsymbol symbolcolor.colors symbol) symbol (index + 1) symbolscolors
                )
                sign.syms
    in
        { sign
            | syms = syms
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
