module SWEditor.EditorSymbol
    exposing
        ( symbolId
        , colorsymbol
        , sizesymbol
        , adjustposition
        )

import SW.Types exposing (Colors)
import SW.Symbol exposing (Symbol)


symbolId : Maybe Symbol -> Int
symbolId symbol =
    case symbol of
        Nothing ->
            0

        Just symb ->
            symb.id


colorsymbol : Colors -> Symbol -> Symbol
colorsymbol colors symbol =
    if colors.nbcolor == Nothing && colors.nwcolor == Nothing then
        symbol
    else
        let
            nbcolor =
                Maybe.withDefault symbol.nbcolor colors.nbcolor

            nwcolor =
                Maybe.withDefault symbol.nwcolor colors.nwcolor
        in
            { symbol | nbcolor = nbcolor, nwcolor = nwcolor }


sizesymbol : { size : Float, pos : Int, adjustment : { x : Int, y : Int } } -> Symbol -> Symbol
sizesymbol size symbol =
    { symbol | size = size.size }


adjustposition : { size : Float, pos : Int, adjustment : { x : Int, y : Int } } -> Symbol -> Symbol
adjustposition size symbol =
    { symbol | x = symbol.x + size.adjustment.x, y = symbol.y + size.adjustment.y }
