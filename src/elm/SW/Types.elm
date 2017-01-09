module SW.Types exposing (..)

import ParseInt as ParseInt exposing (..)
import String exposing (..)


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
    , size : Float
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



--typerange "hand" -> { start = "100", end = "204" } : { start : String, end : String }


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