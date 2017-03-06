module SWEditor.Drag exposing (..)

import SWEditor.Types exposing (..)
import SWEditor.EditorSign exposing (..)
import SW.Types exposing (Symbol, Sign)


dragsign : Model -> Sign
dragsign model =
    let
        dragoffset =
            Offset (model.xy.x - model.dragstart.x) (model.xy.y - model.dragstart.y)

        sign =
            model.dragsign

        symbols =
            dragSymbols dragoffset sign.syms

        bounds =
            getSignBounding symbols
    in
        { sign | syms = symbols, width = bounds.width, height = bounds.height, x = bounds.x, y = bounds.y }


dragSymbols : Offset -> List Symbol -> List Symbol
dragSymbols offset symbols =
    List.map
        (\symbol ->
            if symbol.selected then
                { symbol | x = symbol.x + offset.offsetx, y = symbol.y + offset.offsety }
            else
                symbol
        )
        symbols
