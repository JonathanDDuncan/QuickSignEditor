module SWEditor.RectangleSelect exposing (..)

import SWEditor.Types exposing (..)
import SWEditor.Rectangle exposing (..)


rectangleselect : Model -> EditorSign
rectangleselect model =
    selectSymbolsIntersection (rectangleSelect model) model


rectangleSelect : Model -> Rect
rectangleSelect model =
    let
        x1 =
            min model.rectanglestart.x model.rectangleend.x

        x2 =
            max model.rectanglestart.x model.rectangleend.x

        y1 =
            min model.rectanglestart.y model.rectangleend.y

        y2 =
            max model.rectanglestart.y model.rectangleend.y
    in
        { x = x1
        , y = y1
        , width = x2 - x1
        , height = y2 - y1
        }


selectSymbolsIntersection : Rect -> Model -> EditorSign
selectSymbolsIntersection rectangle model =
    let
        sign =
            model.sign
    in
        { sign | syms = List.map (selectIntersected rectangle) sign.syms }


selectIntersected : Rect -> EditorSymbol -> EditorSymbol
selectIntersected rectangle symbol =
    let
        symbolrect =
            getsymbolRectangle symbol

        selectRectangle =
            { rectangle
                | x =
                    rectangle.x
                , y =
                    rectangle.y
            }
    in
        if intersect selectRectangle symbolrect then
            { symbol | selected = True }
        else
            symbol
