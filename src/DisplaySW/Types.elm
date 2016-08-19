module DisplaySW.Types exposing (..)

-- import SubDisplaySWs.Types


type alias Model =
    { word : String
    , suggestions : String
    }


type Msg
    = Change String
    | Check
    | Suggest String



-- Plus any other types unique to this DisplaySW
-- Plus any library function to run on the types
