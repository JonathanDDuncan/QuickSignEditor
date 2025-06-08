module SWEditor.Drag exposing (dragsign)

import SWEditor.Types exposing (Model, Offset)
import SW.Sign exposing (Sign)
import SW.Symbol exposing (Symbol)
import SW.Rectangle exposing (getBounding)


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
            getBounding symbols
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
