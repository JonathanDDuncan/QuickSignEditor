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

idrecord id a = {a  | id = id}
-- Plus any other types unique to this DisplaySW
-- Plus any library function to run on the types
