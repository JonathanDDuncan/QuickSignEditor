module DisplaySW.Types exposing (..)

-- import SubDisplaySWs.Types


type alias Model =
    { word : String
    , suggestions : Sign
    }


type Msg
    = Change String
    | RequestSign
    | SetSign Sign


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
    , fontsize : Int
    , nwcolor : String
    , pua : String
    , code : Int
    , key : String
    , nbcolor : String
    }



-- Plus any other types unique to this DisplaySW
-- Plus any library function to run on the types
