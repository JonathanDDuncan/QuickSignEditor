module SW.Sign
    exposing
        ( Sign
        , Lane(..)
        , signinit
        , getlane
        , lanes
        , lastsignid
        , refreshsign
        , centerSignSmallest
        , centerSign
        , centersignarea
        )

import SW.Symbol exposing (Symbol, moveSymbols)
import SW.Identifier exposing (updateIds, lastid)
import SW.Rectangle exposing (Rect, getBounding)


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


updatesignids : Int -> Sign -> Sign
updatesignids idstart sign =
    { sign | syms = updateIds idstart sign.syms }


boundsign : Sign -> Sign
boundsign sign =
    let
        boundingbox =
            getBounding sign.syms
    in
        { sign
            | width = boundingbox.width
            , height = boundingbox.height
            , x = boundingbox.x
            , y = boundingbox.y
        }


refreshsign : Int -> Sign -> Sign
refreshsign idstart sign =
    updatesignids idstart sign
        |> boundsign
        |> centerSignSmallest


centerSign : Int -> Int -> Sign -> Sign
centerSign desiredxcenter desiredycenter sign =
    let
        bounding =
            getBounding sign.syms

        movex =
            desiredxcenter - (bounding.x + bounding.width // 2)

        movey =
            desiredycenter - (bounding.y + bounding.height // 2)

        movedsymbols =
            moveSymbols movex movey sign.syms

        newbounding =
            getBounding movedsymbols
    in
        { sign
            | width = newbounding.width
            , height = newbounding.height
            , x = newbounding.x
            , y = newbounding.y
            , syms = movedsymbols
        }


centersignarea : { a | height : Int, width : Int } -> Sign -> Sign
centersignarea area sign =
    centerSign (area.width // 2) (area.height // 2) sign


centerSignSmallest : Sign -> Sign
centerSignSmallest sign =
    let
        bounding =
            getBounding sign.syms
    in
        centerSign (bounding.width // 2) (bounding.height // 2) sign
