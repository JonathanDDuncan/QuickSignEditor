module Choosers.HandPng exposing (..)

import Choosers.Types exposing (..)
import String exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Choosers.Types exposing (..)


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
                "rotate(" ++ (toString handpng.rotate) ++ "deg)"
            else
                ""

        transformvalue =
            (moretransform) ++ " " ++ (miror) ++ " " ++ (rotate) ++ ";"

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
                    ++ (morestyle)
                    ++ (transform)
                )
            ]
            []


gethandpng : String -> Int -> Int -> HandFills -> HandPng
gethandpng key rotation fill filltype =
    let
        handtype =
            gethandtype filltype

        pngcss1 =
            handpngcss key fill filltype

        rotate =
            gethandpngrotation rotation handtype

        miror =
            gethandpngmiror handtype
    in
        { pngcss = pngcss1, rotate = rotate, miror = miror }


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
    if filltype == LeftBabyEdge || filltype == RightBabyEdge then
        ""
    else
        String.toLower "hands-" ++ toLower (String.slice 1 4 symbolkey) ++ toString (fill - 1) ++ "0"
