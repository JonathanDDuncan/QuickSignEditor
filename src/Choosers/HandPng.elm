module Choosers.HandPng exposing (handpngspan, gethandpng)

import Choosers.Types exposing (Model, Msg, HandPng)
import SW.Symbol as HandFills exposing (HandFills)
import SW.Symbol exposing (Hands(Right, Left), gethandtype)
import String exposing (toLower)
import Html exposing (Html)
import Html.Attributes exposing (class, attribute)


handpngspan : { a | miror : Bool, pngcss : String, rotate : number } -> String -> String -> String -> String -> Html c
handpngspan handpng classes morestyle moretransform displaystyle =
    let
        miror =
            if handpng.miror then
                "scaleX(-1)"
            else
                ""

        rotate =
            if handpng.rotate /= 0 then
                "rotate(" ++ toString handpng.rotate ++ "deg)"
            else
                ""

        transformvalue =
            moretransform ++ " " ++ miror ++ " " ++ rotate ++ ";"

        transform =
            if String.length transformvalue > 3 then
                "transform: " ++ transformvalue
            else
                ""
    in
        Html.div
            [ class (handpng.pngcss ++ " " ++ classes)
            , attribute "style"
                ("display:"
                    ++ displaystyle
                    ++ ";"
                    ++ morestyle
                    ++ transform
                )
            ]
            []


gethandpng : String -> Int -> Int -> HandFills -> HandPng
gethandpng key rotation fill filltype =
    { pngcss = handpngcss key fill filltype
    , rotate =
        gethandtype filltype
            |> gethandpngrotation rotation
    , miror =
        gethandtype filltype
            |> gethandpngmiror
    }


gethandpngrotation : Int -> Hands -> Int
gethandpngrotation rotation handtype =
    case handtype of
        Right ->
            case rotation of
                2 ->
                    315

                3 ->
                    270

                4 ->
                    225

                5 ->
                    180

                6 ->
                    135

                7 ->
                    90

                8 ->
                    45

                _ ->
                    0

        Left ->
            case rotation of
                2 ->
                    45

                3 ->
                    90

                4 ->
                    135

                5 ->
                    180

                6 ->
                    225

                7 ->
                    270

                8 ->
                    315

                9 ->
                    0

                10 ->
                    315

                11 ->
                    270

                12 ->
                    225

                13 ->
                    180

                14 ->
                    135

                15 ->
                    90

                16 ->
                    45

                _ ->
                    0


gethandpngmiror : Hands -> Bool
gethandpngmiror handtype =
    if handtype == Right then
        False
    else
        True


handpngcss : String -> Int -> HandFills -> String
handpngcss symbolkey fill filltype =
    if filltype == HandFills.LeftBabyEdge || filltype == HandFills.RightBabyEdge then
        ""
    else
        String.toLower "hands-" ++ toLower (String.slice 1 4 symbolkey) ++ toString (fill - 1) ++ "0"
