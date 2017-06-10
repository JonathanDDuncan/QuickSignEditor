module SWEditor.Select exposing (unselectSignSymbols, unselectSymbols, selectSymbolId)

import SWEditor.Types exposing (Model, Msg)
import SW.Sign exposing (Sign)
import SW.Symbol exposing (Symbol)


unselectSignSymbols : Sign -> Sign
unselectSignSymbols sign =
    { sign | syms = unselectSymbols sign.syms }


unselectSymbols : List Symbol -> List Symbol
unselectSymbols symbols =
    List.map unselectSymbol symbols


unselectSymbol : Symbol -> Symbol
unselectSymbol symbol =
    { symbol | selected = False }


selectSymbolId : Int -> Model -> Sign
selectSymbolId id model =
    let
        sign =
            model.sign
    in
        { sign | syms = selectId id sign.syms }


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
