module SWEditor.Select exposing (..)

import SWEditor.Types exposing (..)
import SWEditor.EditorSign exposing (..)
import SWEditor.EditorSymbol exposing (..)


unselectSymbols : EditorSign -> EditorSign
unselectSymbols sign =
    { sign | syms = List.map unselectSymbol sign.syms }


unselectSymbol : EditorSymbol -> EditorSymbol
unselectSymbol symbol =
    { symbol | selected = False }


selectSymbolId : Int -> Model -> EditorSign
selectSymbolId id model =
    let
        sign =
            model.sign

        symbols =
            selectId id sign.syms
    in
        { sign | syms = symbols }


selectId : Int -> List EditorSymbol -> List EditorSymbol
selectId id symbols =
    List.map
        (\symbol ->
            if symbol.id == id then
                { symbol | selected = True }
            else
                symbol
        )
        symbols
