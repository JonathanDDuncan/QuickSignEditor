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

        coordinatestr =
            getcoordinatestr fsw

        coordinateresult =
            getcooordinate coordinatestr

        symbolsstrings =
            getsymbolsstrings fsw

        symsresult =
            createsymbols symbolsstrings

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


createsymbols : List String -> Result String (List SWEditor.EditorSymbol.EditorSymbol)
createsymbols symbolsstrings =
    List.map createsymbol symbolsstrings
        |> combine


createsymbol : String -> Result String SWEditor.EditorSymbol.EditorSymbol
createsymbol symbolstring =
    let
        key =
            Regex.find (Regex.AtMost 1) (regex (re_sym ++ re_coord)) symbolstring
                |> matchestostrings
                |> List.head
                |> Result.fromMaybe (couldnoterror "get key" symbolstring)

        coordinatestr =
            getcoordinatestr symbolstring

        symbolonly =
            applyToOkValue (\key1 -> getSymbolEditorKey key1 partialsymbolsizes |> Ok) key

        coordinateresult =
            getcooordinate coordinatestr

        symbol =
            symbolonly
                |> Result.andThen
                    (setresultvalue coordinateresult (\symbol value -> { symbol | x = value.x, y = value.y }))
    in
        symbol


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


getlane : String -> Result String Lane
getlane fsw =
    let
        laneandcoordinate =
            getlaneandcoordinate fsw

        lanestring =
            applyToOkValue (\value -> String.left 1 value |> Ok) laneandcoordinate
    in
        applyToOkValue
            (\lanestringvalue ->
                List.filter (\( str, lane ) -> str == lanestringvalue) lanes
                    |> List.map (\( str, lane ) -> lane)
                    |> List.head
                    |> Maybe.withDefault MiddleLane
                    |> Ok
            )
            lanestring


getcooordinate : Result String String -> Result String { x : Int, y : Int }
getcooordinate coordinatestr =
    let
        coordinatelistresult =
            getcoordinatelist coordinatestr

        xresult =
            applyToOkValueAppendMsg (couldnoterror "get x coordinate" <| Result.withDefault "" coordinatestr)
                (\value ->
                    value
                        |> List.head
                        |> toInt
                )
                coordinatelistresult

        yresult =
            applyToOkValueAppendMsg (couldnoterror "get y coordinate" <| Result.withDefault "" coordinatestr)
                (\value ->
                    value
                        |> List.drop 1
                        |> List.head
                        |> toInt
                )
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
        coordinatestring =
            getcoordinate coordinatestr

        goodcoordinatestring =
            createrule (\value -> expectederror "Sign coordinate" value "to be 7 characters long") (\value -> String.length value == 7) coordinatestring

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
                            Err <| couldnoterror "split coordinate value into two pieces on 'x'" value

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
                |> Ok
        )
        stringcoordinate


getlaneandcoordinate : String -> Result String String
getlaneandcoordinate fsw =
    let
        symbolsplit =
            Regex.find All (regex re_lanecoord) fsw
                |> matchestostrings
    in
        List.filter (\item -> startwithlanevalue item) symbolsplit
            |> List.head
            |> Result.fromMaybe (couldnoterror "find lane and coordinate" fsw)


startwithlanevalue : String -> Bool
startwithlanevalue item =
    List.any (\lv -> String.startsWith lv item) laneValues


laneValues : List String
laneValues =
    List.map (\( value, lane ) -> value) lanes


partialsymbolsizes : Dict.Dict String Size
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


createrule : (value -> a) -> (value -> Bool) -> Result a value -> Result a value
createrule errormessage test result =
    applyToOkValue (\value -> rule errormessage test value) result


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
    (\recd -> applyToOkValue (\value -> setter recd value |> Ok) result)


toInt : Maybe String -> Result String Int
toInt str =
    str
        |> Result.fromMaybe "Cannot convert Nothing to Int"
        |> Result.andThen String.toInt


combine : List (Result x a) -> Result x (List a)
combine =
    List.foldr (Result.map2 (::)) (Ok [])


applyToOkValue : (a -> Result b value) -> Result b a -> Result b value
applyToOkValue callback result =
    case result of
        Ok value ->
            callback value

        Err msg ->
            Err msg


applyToOkValueAppendMsg : String -> (a -> Result String value) -> Result String a -> Result String value
applyToOkValueAppendMsg message callback result =
    case result of
        Ok value ->
            callback value

        Err msg ->
            Err <| message ++ " | " ++ msg



-- Regex


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
