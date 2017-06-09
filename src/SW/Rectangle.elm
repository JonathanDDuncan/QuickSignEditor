module SW.Rectangle exposing (Rect, rect, minrectangle, intersect)


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


rect : Int -> Int -> Int -> Int -> Rect
rect px1 px2 py1 py2 =
    let
        x1 =
            min px1 px2

        x2 =
            max px1 px2

        y1 =
            min py1 py2

        y2 =
            max py1 py2
    in
        { x = x1
        , y = y1
        , width = x2 - x1
        , height = y2 - y1
        }


minrectangle : Rect -> Int -> Int -> Rect
minrectangle rectangle minwidth minheight =
    let
        newwidth =
            if abs rectangle.width < minwidth then
                minwidth
            else
                rectangle.width

        newheigth =
            if abs rectangle.height < minheight then
                minheight
            else
                rectangle.height
    in
        { rectangle | width = newwidth, height = newheigth }
