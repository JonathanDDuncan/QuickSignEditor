module ViewHelper.ViewExtra exposing (..)


px : Int -> String
px number =
    toString number ++ "px"


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


shrinkdontzoom : Float -> Float -> Float -> Float -> Float
shrinkdontzoom itemwidth itemheight finalwidth finalheight =
    let
        calculatedscale =
            calculatescale itemwidth itemheight finalwidth finalheight
    in
        if calculatedscale > 1 then
            1
        else
            calculatedscale


calculatescale : Float -> Float -> Float -> Float -> Float
calculatescale contentWidth contentHeight availableWidth availableHeight =
    Basics.min (availableWidth / contentWidth) (availableHeight / contentHeight)


transformscale : a -> ( String, String )
transformscale scale =
    ( "transform", ("scale(" ++ toString scale ++ ")") )
