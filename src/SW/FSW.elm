module SW.FSW exposing (..)

import SWEditor.EditorSign exposing (..)
import Regex exposing (..)
import SWEditor.EditorSymbol exposing (getSymbolbyKey)
import Dict
import SW.Types exposing (..)


toEditorSign : Dict.Dict String Size -> String -> Result String EditorSign
toEditorSign symbolsizes fsw =
    let
        laneresult =
            getlane fsw

        coordinatestr =
            getcoordinatestr fsw

        coordinateresult =
            getcooordinate coordinatestr

        symbolsstrings =
            getsymbolsstrings fsw

        symsresult =
            createsymbols symbolsizes symbolsstrings

        sign =
            Ok signinit
                |> Result.andThen
                    (setresultvalue coordinateresult (\sign value -> { sign | x = value.x, y = value.y }))
                |> Result.andThen
                    (setresultvalue laneresult (\sign value -> { sign | lane = value }))
                |> Result.andThen
                    (setresultvalue symsresult (\sign value -> { sign | syms = value }))
    in
        sign


createsymbols : Dict.Dict String Size -> List String -> Result String (List SWEditor.EditorSymbol.Symbol)
createsymbols symbolsizes symbolsstrings =
    List.map (createsymbol symbolsizes) symbolsstrings
        |> combine


createsymbol : Dict.Dict String Size -> String -> Result String SWEditor.EditorSymbol.Symbol
createsymbol symbolsizes symbolstring =
    let
        keystr =
            getkeystr symbolstring

        coordinatestr =
            getcoordinatestr symbolstring

        symbolonly =
            Result.andThen (\key1 -> getSymbolbyKey key1 symbolsizes |> Ok) keystr

        coordinateresult =
            getcooordinate coordinatestr

        symbol =
            symbolonly
                |> Result.andThen
                    (setresultvalue coordinateresult (\symbol value -> { symbol | x = value.x, y = value.y }))
    in
        symbol


getlane : String -> Result String Lane
getlane fsw =
    let
        laneandcoordinate =
            getlanestr fsw

        lanestring =
            Result.andThen (\value -> String.left 1 value |> Ok) laneandcoordinate
    in
        Result.andThen
            (\lanestringvalue ->
                List.filter (\( str, lane ) -> str == lanestringvalue) lanes
                    |> List.map (\( str, lane ) -> lane)
                    |> List.head
                    |> Result.fromMaybe (couldnoterror "get lane" lanestringvalue)
            )
            lanestring


getcooordinate : Result String String -> Result String { x : Int, y : Int }
getcooordinate coordinatestr =
    let
        coordinatelistresult =
            getcoordinatelist coordinatestr

        xresult =
            applyToOkValueAppendMsg (couldnoterror "get x coordinate" <| Result.withDefault "" coordinatestr)
                (\value -> value |> List.head |> toInt)
                coordinatelistresult

        yresult =
            applyToOkValueAppendMsg (couldnoterror "get y coordinate" <| Result.withDefault "" coordinatestr)
                (\value -> value |> List.drop 1 |> List.head |> toInt)
                coordinatelistresult

        coordinate =
            (Ok { x = 0, y = 0 })
                |> Result.andThen
                    (setresultvalue xresult (\position value -> { position | x = value }))
                |> Result.andThen
                    (setresultvalue yresult (\position value -> { position | y = value }))
    in
        coordinate


getcoordinatelist : Result String String -> Result String (List String)
getcoordinatelist coordinatestr =
    let
        goodcoordinatestring =
            createrule coordinatestr
                (\value -> String.length value == 7)
                (\value -> expectederror "Sign coordinate" value "to be 7 characters long")

        coordinatelist =
            Result.andThen splitcoordinatestring goodcoordinatestring
    in
        coordinatelist


splitcoordinatestring : String -> Result String (List String)
splitcoordinatestring str =
    let
        split =
            String.split "x" str
                |> Ok
    in
        createrule split
            (\value -> List.length value == 2)
            (\value -> couldnoterror "split coordinate value into two pieces on 'x'" str)


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



-- Error Message


expectederror : String -> String -> String -> String
expectederror what value expectation =
    what ++ " '" ++ value ++ "' " ++ expectation ++ "."


couldnoterror : String -> String -> String
couldnoterror action source =
    "Could not " ++ action ++ " from '" ++ source ++ "'."



-- Rules


createrule : Result a value -> (value -> Bool) -> (value -> a) -> Result a value
createrule result test errormessage =
    Result.andThen (\value -> rule errormessage test value) result


rule : (value -> a) -> (value -> Bool) -> value -> Result a value
rule errormessage test value =
    if test value then
        Ok value
    else
        Err <| errormessage value



-- Tuples


(=>) : a -> b -> ( a, b )
(=>) =
    (,)



-- Result


setresultvalue : Result a b -> (c -> b -> value) -> c -> Result a value
setresultvalue result setter =
    (\recd -> Result.andThen (\value -> setter recd value |> Ok) result)


toInt : Maybe String -> Result String Int
toInt str =
    str
        |> Result.fromMaybe "Cannot convert Nothing to Int"
        |> Result.andThen String.toInt


combine : List (Result x a) -> Result x (List a)
combine =
    List.foldr (Result.map2 (::)) (Ok [])


applyToOkValueAppendMsg : String -> (a -> Result String value) -> Result String a -> Result String value
applyToOkValueAppendMsg message callback result =
    case result of
        Ok value ->
            callback value

        Err msg ->
            Err <| message ++ " | " ++ msg



-- Regex


getkeystr : String -> Result String String
getkeystr str =
    Regex.find (Regex.AtMost 1) (regex (re_sym)) str
        |> matchestostrings
        |> List.head
        |> Result.fromMaybe (couldnoterror "get key" str)


getlanestr : String -> Result String String
getlanestr fsw =
    let
        symbolsplit =
            Regex.find All (regex re_lane) fsw
                |> matchestostrings
    in
        List.filter (\item -> startwithlanevalue item) symbolsplit
            |> List.head
            |> Result.fromMaybe (couldnoterror "find lane" fsw)


getcoordinatestr : String -> Result String String
getcoordinatestr str =
    Regex.find (Regex.AtMost 1) (regex re_coord) str
        |> matchestostrings
        |> List.head
        |> Result.fromMaybe (couldnoterror "get coordinate" str)


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


re_lane : String
re_lane =
    "[BLMR]"


re_lanecoord : String
re_lanecoord =
    re_lane ++ "(" ++ re_coord ++ ")"


re_word : String
re_word =
    re_lane ++ "(" ++ re_coord ++ ")(" ++ re_sym ++ re_coord ++ ")*"


re_term : String
re_term =
    "(A(" ++ re_sym ++ ")+)"
