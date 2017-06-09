module SW.Rectangle exposing (Rect, rect, minrectangle, intersect, getBounding)


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


getBounding :
    List { a | height : Int, size : Float, width : Int, x : Int, y : Int }
    -> Rect
getBounding symbols =
    let
        maxvalue =
            if List.length symbols == 0 then
                0
            else
                10000

        x1 =
            List.foldr (\s -> min s.x) maxvalue symbols

        y1 =
            List.foldr (\s -> min s.y) maxvalue symbols

        x2 =
            List.foldr (\s -> max (s.x + round (toFloat s.width * s.size))) 0 symbols

        y2 =
            List.foldr (\s -> max (s.y + round (toFloat s.height * s.size))) 0 symbols
    in
        Rect x1 y1 (x2 - x1) (y2 - y1)
