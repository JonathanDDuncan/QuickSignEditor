module SW.Types exposing (..)

import ParseInt as ParseInt exposing (..)
import String exposing (..)


-- import SubDisplaySWs.Types


type alias Sign =
    { width : Int
    , height : Int
    , x : Int
    , y : Int
    , backfill : String
    , syms : List Symbol
    }


type alias Symbol =
    { key : String
    , id : Int
    , selected : Bool
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    , size : Float
    , nwcolor : String
    , nbcolor : String
    }


symbolinit : Symbol
symbolinit =
    { key = ""
    , id = 0
    , selected = False
    , x = 0
    , y = 0
    , width = 0
    , height = 0
    , size = 1
    , nwcolor = ""
    , nbcolor = ""
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


type alias Base =
    Int


type alias Fill =
    Int


type alias Rotation =
    Int


type alias Code =
    Int


type alias Key =
    String


type alias TypeRange =
    { start : String
    , end : String
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
            l_ :: ls_ ->
                Just <| List.foldl (maxBy f) l_ ls_

            _ ->
                Nothing


typerange : String -> TypeRange
typerange typename =
    case typename of
        "writing" ->
            { start = "100"
            , end = "37e"
            }

        "hand" ->
            { start = "100"
            , end = "204"
            }

        "movement" ->
            { start = "205"
            , end = "2f6"
            }

        "dynamic" ->
            { start = "2f7"
            , end = "2fe"
            }

        "head" ->
            { start = "2ff"
            , end = "36c"
            }

        "hcenter" ->
            { start = "2ff"
            , end = "36c"
            }

        "vcenter" ->
            { start = "2ff"
            , end = "375"
            }

        "trunk" ->
            { start = "36d"
            , end = "375"
            }

        "limb" ->
            { start = "376"
            , end = "37e"
            }

        "location" ->
            { start = "37f"
            , end = "386"
            }

        "punctuation" ->
            { start = "387"
            , end = "38b"
            }

        _ ->
            { start = "100"
            , end = "38b"
            }


iskey : String -> String -> Bool
iskey key typename =
    let
        range =
            typerange typename

        start =
            ParseInt.parseIntHex range.start
                |> Result.toMaybe
                |> Maybe.withDefault 0

        end =
            ParseInt.parseIntHex range.end
                |> Result.toMaybe
                |> Maybe.withDefault 0

        char =
            String.slice 1 4 key
                |> ParseInt.parseIntHex
                |> Result.toMaybe
                |> Maybe.withDefault 0
    in
        start <= char && end >= char



--iskey "S100" "hand" -> True
--iskey "S100" "head" -> False


getvalidfills : String -> List Fill
getvalidfills validfillsstring =
    case validfillsstring of
        "1 - 6" ->
            List.range 1 6

        "1 - 4" ->
            List.range 1 4

        "1, 2" ->
            List.range 1 2

        "1 - 3" ->
            List.range 1 3

        "1 - 5" ->
            List.range 1 5

        "1" ->
            [ 1 ]

        "2" ->
            [ 2 ]

        _ ->
            let
                a =
                    Debug.log "Could not match valid fills string" validfillsstring
            in
                []


getvalidrotations : String -> List Rotation
getvalidrotations validrotationsstring =
    case validrotationsstring of
        "1 - 16" ->
            List.range 1 16

        "1 - 8" ->
            List.range 1 8

        "1" ->
            [ 1 ]

        "1 - 4" ->
            List.range 1 4

        "1, 2, 4, 5, 6, 8" ->
            [ 1, 2, 4, 5, 6, 8 ]

        "1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 14, 16" ->
            [ 1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 14, 16 ]

        "1 - 6" ->
            List.range 1 6

        "1, 2" ->
            List.range 1 2

        "1 - 9" ->
            List.range 1 9

        _ ->
            let
                a =
                    Debug.log "Could not match valid rotations string" validrotationsstring
            in
                []


isValidRotation : Int -> List Int -> Bool
isValidRotation rotation validrotations =
    List.any ((==) rotation) validrotations
