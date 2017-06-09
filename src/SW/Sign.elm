module SW.Sign
    exposing
        ( Sign
        , Lane(..)
        , signinit
        , getlane
        , lanes
        , lastsignid
        )

import SW.Symbol exposing (Symbol)
import SW.Identifier exposing (lastid)


type alias Sign =
    { lane : Lane
    , width : Int
    , height : Int
    , x : Int
    , y : Int
    , backfill : String
    , syms : List Symbol
    , spelling : String
    }


type Lane
    = BLane
    | LeftLane
    | MiddleLane
    | RightLane


signinit : Sign
signinit =
    { lane = MiddleLane
    , width = 0
    , height = 0
    , x = 0
    , y = 0
    , backfill = ""
    , syms = []
    , spelling = ""
    }


getlane : String -> Maybe Lane
getlane lanestringvalue =
    List.filter (\( str, _ ) -> str == lanestringvalue) lanes
        |> List.map (\( _, lane ) -> lane)
        |> List.head


lanes : List ( String, Lane )
lanes =
    [ ( "B", BLane )
    , ( "L", LeftLane )
    , ( "M", MiddleLane )
    , ( "R", RightLane )
    ]


lastsignid : { b | syms : List { a | id : Int } } -> Int
lastsignid sign =
    lastid sign.syms
