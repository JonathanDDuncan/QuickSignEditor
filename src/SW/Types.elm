module SW.Types exposing (..)

-- import SubDisplaySWs.Types


type alias Sign =
    { width : Int
    , height : Int
    , text : String
    , x : Int
    , y : Int
    , backfill : String
    , syms : List Symbol
    , laned : Bool
    , left : Int
    }


type alias Symbol =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , fontsize : Int
    , nwcolor : String
    , pua : String
    , code : Int
    , key : String
    , nbcolor : String
    }


type alias Selectable a =
    { a | selected : Bool }


type alias Idable a =
    { a | id : Int }


type alias NamedPosition =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , name : String
    }


{-| The position of the mouse relative to the whole document. So if you are
scrolled down a bunch, you are still getting a coordinate relative to the
very top left corner of the *whole* document.
-}
type alias Position =
    { x : Int
    , y : Int
    }



-- Plus any other types unique to this DisplaySW
-- Plus any library function to run on the types


maximumBy : (a -> comparable) -> List a -> Maybe a
maximumBy f ls =
    let
        maxBy f x y =
            if (f x) > (f y) then
                x
            else
                y
    in
        case ls of
            l' :: ls' ->
                Just <| List.foldl (maxBy f) l' ls'

            _ ->
                Nothing
