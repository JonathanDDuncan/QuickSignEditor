module SWEditor.RectangleSelect exposing (rectangleselect, rectangleStartCurrent)

import SWEditor.Types exposing (Model, Msg(..))
import SWEditor.Rectangle exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SW.Types exposing (Symbol, Sign)


rectangleselect : Model -> Sign
rectangleselect model =
    selectSymbolsIntersection (rectangleStartCurrent model) model.sign


rectangleStartCurrent : Model -> Rect
rectangleStartCurrent model =
    let
        rectangle =
            rect model.rectanglestart.x model.xy.x model.rectanglestart.y model.xy.y

        minimumsizerectangle =
            minrectangle rectangle 1 1
    in
        minimumsizerectangle


selectSymbolsIntersection : Rect -> Sign -> Sign
selectSymbolsIntersection rectangle sign =
    { sign | syms = List.map (selectIntersected rectangle) sign.syms }


selectIntersected : Rect -> Symbol -> Symbol
selectIntersected rectangle symbol =
    if intersect rectangle (getsymbolRectangle symbol) then
        { symbol | selected = True }
    else
        symbol
