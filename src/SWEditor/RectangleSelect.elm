module SWEditor.RectangleSelect exposing (..)

import SWEditor.Types exposing (..)
import SWEditor.Rectangle exposing (..)
import SWEditor.EditorSign exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SW.Types exposing (Symbol)


rectangleselect : Model -> EditorSign
rectangleselect model =
    selectSymbolsIntersection (rectangleStartCurrent model) model.sign


rectangleStartCurrent : Model -> Rect
rectangleStartCurrent model =
    rect model.rectanglestart.x model.xy.x model.rectanglestart.y model.xy.y


selectSymbolsIntersection : Rect -> EditorSign -> EditorSign
selectSymbolsIntersection rectangle sign =
    { sign | syms = List.map (selectIntersected rectangle) sign.syms }


selectIntersected : Rect -> Symbol -> Symbol
selectIntersected rectangle symbol =
    if intersect rectangle (getsymbolRectangle symbol) then
        { symbol | selected = True }
    else
        symbol
