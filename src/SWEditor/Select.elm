module SWEditor.Select exposing (..)

import SWEditor.Types exposing (..)
import SW.Types exposing (Symbol, Sign)


unselectSymbols : Sign -> Sign
unselectSymbols sign =
    { sign | syms = List.map unselectSymbol sign.syms }


unselectSymbol : Symbol -> Symbol
unselectSymbol symbol =
    { symbol | selected = False }


selectSymbolId : Int -> Model -> Sign
selectSymbolId id model =
    let
        sign =
            model.sign

        symbols =
            selectId id sign.syms
    in
        { sign | syms = symbols }


selectId : Int -> List Symbol -> List Symbol
selectId id symbols =
    List.map
        (\symbol ->
            if symbol.id == id then
                { symbol | selected = True }
            else
                symbol
        )
        symbols
