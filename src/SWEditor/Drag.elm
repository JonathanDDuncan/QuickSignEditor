module SWEditor.Drag exposing (dragsign)

import SWEditor.Types exposing (Model, Msg, Offset)
import SWEditor.EditorSign exposing (getSignBounding)
import SW.Types exposing (Sign)
import SW.Symbol exposing (Symbol)


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
