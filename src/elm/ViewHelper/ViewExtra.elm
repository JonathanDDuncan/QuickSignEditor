module ViewHelper.ViewExtra exposing (..)


px : Int -> String
px number =
    toString number ++ "px"


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
