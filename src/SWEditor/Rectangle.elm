module SWEditor.Rectangle exposing (..)


type alias Rect =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


intersect : Rect -> Rect -> Bool
intersect rect1 rect2 =
    let
        rect1x2 =
            rect1.x + rect1.width

        rect1y2 =
            rect1.y + rect1.height

        rect2x2 =
            rect2.x + rect2.width

        rect2y2 =
            rect2.y + rect2.height
    in
        rect1.x < rect2x2 && rect1x2 > rect2.x && rect1.y < rect2y2 && rect1y2 > rect2.y
