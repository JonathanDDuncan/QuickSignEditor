module SW.Types exposing (Position, NamedPosition, Size, Colors)


type alias Colors =
    { nbcolor : Maybe String
    , nwcolor : Maybe String
    }


type alias NamedPosition =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , name : String
    }


type alias Size =
    { width : Int
    , height : Int
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
