module SW.FSW exposing (..)

import SWEditor.EditorSign exposing (..)
import Regex exposing (..)
import SWEditor.EditorSymbol exposing (getSymbolEditorKey)
import Dict
import SW.Types exposing (..)


toEditorSign : String -> Result String EditorSign
toEditorSign fsw =
    let
        laneresult =
            getlane fsw

        lanecoord =
            getlaneandcoordinate fsw

        xresult =
            getx lanecoord

        yresult =
            gety lanecoord

        symbolsstrings =
            getsymbolsstrings fsw

        symsresult =
            createsymbols symbolsstrings

        sign =
            Ok signinit
                |> Result.andThen
                    (setresultvalue xresult (\sign value -> { sign | x = value }))
                |> Result.andThen
                    (setresultvalue yresult (\sign value -> { sign | y = value }))
                |> Result.andThen
                    (setresultvalue laneresult (\sign value -> { sign | lane = value }))
                |> Result.andThen
                    (setresultvalue symsresult (\sign value -> { sign | syms = value }))
    in
        sign


createsymbols symbolsstrings =
    List.map createsymbol symbolsstrings
        |> combine


combine : List (Result x a) -> Result x (List a)
combine =
    List.foldr (Result.map2 (::)) (Ok [])


createsymbol symbolstring =
    let
        key =
            Regex.find (Regex.AtMost 1) (regex (re_sym ++ re_coord)) symbolstring
                |> matchestostrings
                |> List.head
                |> Result.fromMaybe ("Could not get key from '" ++ symbolstring ++ "'")

        coordinate =
            Regex.find (Regex.AtMost 1) (regex re_coord) symbolstring
                |> matchestostrings
                |> List.head
                |> Result.fromMaybe ("Could not get coordinate from '" ++ symbolstring ++ "'")

        symbolonly =
            applyToOkValue (\key1 -> getSymbolEditorKey key1 partialsymbolsizes) key

        xresult =
            getx coordinate

        yresult =
            gety coordinate

        symbol =
            symbolonly
                |> Result.andThen
                    (setresultvalue xresult (\sign value -> { sign | x = value }))
                |> Result.andThen
                    (setresultvalue yresult (\sign value -> { sign | y = value }))
    in
        symbol


partialsymbolsizes =
    let
        symbolsizes =
            [ { k = "", w = 0, h = 0 }
            , { k = "", w = 0, h = 0 }
            , { k = "", w = 0, h = 0 }
            ]
    in
        Dict.fromList <|
            List.map (\symbolsize -> (.k symbolsize) => (Size (.w symbolsize) (.h symbolsize))) symbolsizes


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


getsymbolsstrings : String -> List String
getsymbolsstrings fsw =
    Regex.find All (regex (re_sym ++ re_coord)) fsw
        |> matchestostrings


matchestostrings : List { b | match : a } -> List a
matchestostrings matches =
    List.map (\match -> match.match) matches


re_sym : String
re_sym =
    "S[123][0-9a-f]{2}[0-5][0-9a-f]"


re_coord : String
re_coord =
    "[0-9]{3}x[0-9]{3}"


re_lanecoord : String
re_lanecoord =
    "[BLMR](" ++ re_coord ++ ")"


re_word : String
re_word =
    "[BLMR](" ++ re_coord ++ ")(" ++ re_sym ++ re_coord ++ ")*"


re_term : String
re_term =
    "(A(" ++ re_sym ++ ")+)"


setresultvalue : Result a b -> (c -> b -> value) -> c -> Result a value
setresultvalue result setter =
    (\recd -> applyToOkValue (setter recd) result)


getlane : String -> Result String Lane
getlane fsw =
    let
        laneandcoordinate =
            getlaneandcoordinate fsw

        lanestring =
            applyToOkValue (String.left 1) laneandcoordinate
    in
        applyToOkValue
            (\lanestringvalue ->
                List.filter (\( str, lane ) -> str == lanestringvalue) lanes
                    |> List.map (\( str, lane ) -> lane)
                    |> List.head
                    |> Maybe.withDefault MiddleLane
            )
            lanestring


applyToOkValue : (a -> value) -> Result b a -> Result b value
applyToOkValue callback result =
    case result of
        Ok value ->
            Ok <| callback value

        Err msg ->
            Err msg


applyToOkValueAppendMsg : String -> (a -> value) -> Result String a -> Result String value
applyToOkValueAppendMsg message callback result =
    case result of
        Ok value ->
            Ok <| callback value

        Err msg ->
            Err <| message ++ " | " ++ msg


getx : Result String String -> Result String Int
getx coordinatestr =
    let
        coordinatelistresult =
            getcoordinatelist coordinatestr
    in
        applyToOkValueAppendMsg ("Could not get x coordinate of '" ++ Result.withDefault "" coordinatestr ++ "'")
            (\value ->
                value
                    |> List.head
                    |> toValueDefaultZero
            )
            coordinatelistresult


gety : Result String String -> Result String Int
gety coordinatestr =
    let
        coordinatelistresult =
            getcoordinatelist coordinatestr
    in
        applyToOkValueAppendMsg ("Could not get y coordinate of '" ++ Result.withDefault "" coordinatestr ++ "'")
            (\value ->
                value
                    |> List.drop 1
                    |> List.head
                    |> toValueDefaultZero
            )
            coordinatelistresult


getcoordinatelist : Result String String -> Result String (List String)
getcoordinatelist coordinatestr =
    let
        coordinatestring =
            getcoordinate coordinatestr

        goodcoordinatestring =
            createrule (\value -> "Sign coordinate '" ++ value ++ "'should be 7 characters long.") (\value -> String.length value == 7) coordinatestring

        coordinatelist =
            case goodcoordinatestring of
                Ok value ->
                    let
                        split =
                            String.split "x" value
                    in
                        if List.length split == 2 then
                            Ok split
                        else
                            Err <| "Could not split coordinate value '" ++ value ++ "' into two pieces on 'x'"

                Err msg ->
                    Err msg
    in
        coordinatelist


getcoordinate : Result b String -> Result b String
getcoordinate stringcoordinate =
    applyToOkValue
        (\value ->
            Regex.find (Regex.AtMost 1) (regex re_coord) value
                |> matchestostrings
                |> List.head
                |> Maybe.withDefault ""
        )
        stringcoordinate


createrule : (value -> a) -> (value -> Bool) -> Result a value -> Result a value
createrule errormessage test result =
    applyToOkValuedontreOk (\value -> rule errormessage test value) result


applyToOkValuedontreOk : (a -> Result b value) -> Result b a -> Result b value
applyToOkValuedontreOk callback result =
    case result of
        Ok value ->
            callback value

        Err msg ->
            Err msg


rule : (value -> a) -> (value -> Bool) -> value -> Result a value
rule errormessage test value =
    if test value then
        Ok value
    else
        Err <| errormessage value


toValueDefaultZero : Maybe String -> Int
toValueDefaultZero str =
    str
        |> Maybe.withDefault "0"
        |> String.toInt
        |> Result.withDefault 0


getlaneandcoordinate : String -> Result String String
getlaneandcoordinate fsw =
    let
        symbolsplit =
            Regex.find All (regex re_lanecoord) fsw
                |> matchestostrings
    in
        List.filter (\item -> startwithlanevalue item) symbolsplit
            |> List.head
            |> Result.fromMaybe ("Could not find lane and coordinate: " ++ fsw)


startwithlanevalue : String -> Bool
startwithlanevalue item =
    List.any (\lv -> String.startsWith lv item) laneValues


laneValues : List String
laneValues =
    List.map (\( value, lane ) -> value) lanes


lanes : List ( String, Lane )
lanes =
    [ ( "B", BLane )
    , ( "L", LeftLane )
    , ( "M", MiddleLane )
    , ( "R", RightLane )
    ]
