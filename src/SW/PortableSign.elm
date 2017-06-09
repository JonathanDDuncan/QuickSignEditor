module SW.PortableSign exposing (PortableSign, portableSigntoSign)

import SW.Symbol exposing (Symbol)
import SW.Sign exposing (Sign, Lane(MiddleLane), getlane)


type alias PortableSign =
    { lane : String
    , width : Int
    , height : Int
    , x : Int
    , y : Int
    , backfill : String
    , syms : List Symbol
    }


portableSigntoSign : PortableSign -> Sign
portableSigntoSign portableSign =
    { lane = getlane portableSign.lane |> Maybe.withDefault MiddleLane
    , width = portableSign.width
    , height = portableSign.height
    , x = portableSign.x
    , y = portableSign.y
    , backfill = portableSign.backfill
    , syms = portableSign.syms
    , spelling = ""
    }
