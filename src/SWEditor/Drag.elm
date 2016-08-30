module SWEditor.Drag exposing (..)

import SWEditor.Types exposing (..)


dragsign : Model -> EditorSign
dragsign model =
    let
        dragoffset =
            Offset (model.dragend.x - model.dragstart.x) (model.dragend.y - model.dragstart.y)

        sign =
            model.dragsign

        symbols =
            dragSymbols dragoffset sign.syms
    in
        { sign | syms = symbols }


dragSymbols : Offset -> List EditorSymbol -> List EditorSymbol
dragSymbols offset symbols =
    List.map
        (\symbol ->
            if symbol.selected then
                { symbol | x = symbol.x + offset.offsetx, y = symbol.y + offset.offsety }
            else
                symbol
        )
        symbols
