module SW.Types
    exposing
        ( Lane
        , lanes
        , getlane
        , Sign
        , signinit
        , PortableSign
        , portableSigntoSign
        , Position
        , NamedPosition
        , Size
        , maximumBy
        , Colors
        )

import SW.Symbol exposing (Symbol)


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


type Lane
    = BLane
    | LeftLane
    | MiddleLane
    | RightLane


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
        maxBy f1 x y =
            if f1 x > f1 y then
                x
            else
                y
    in
        case ls of
            l_ :: ls_ ->
                Just <| List.foldl (maxBy f) l_ ls_

            _ ->
                Nothing
