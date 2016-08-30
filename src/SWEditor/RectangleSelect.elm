module SWEditor.RectangleSelect exposing (..)

import SWEditor.Types exposing (..)
import SWEditor.Rectangle exposing (..)


rectangleselect : Model -> EditorSign
rectangleselect model =
    selectSymbolsIntersection (rectangleStartCurrent model) model.sign


rectangleStartCurrent : Model -> Rect
rectangleStartCurrent model =
    rect model.rectanglestart.x model.xy.x model.rectanglestart.y model.xy.y


selectSymbolsIntersection : Rect -> EditorSign -> EditorSign
selectSymbolsIntersection rectangle sign =
    { sign | syms = List.map (selectIntersected rectangle) sign.syms }


selectIntersected : Rect -> EditorSymbol -> EditorSymbol
selectIntersected rectangle symbol =
    let
        symbolrect =
            getsymbolRectangle symbol

        -- selectRectangle =
        --     { rectangle
        --         | x =
        --             rectangle.x
        --         , y =
        --             rectangle.y
        --     }
    in
        if intersect rectangle symbolrect then
            { symbol | selected = True }
        else
            symbol
