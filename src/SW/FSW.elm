module SW.FSW exposing (..)

import Regex exposing (..)
import SWEditor.EditorSymbol exposing (getSymbolbyKey)
import SWEditor.EditorSign exposing (centerSign)
import Dict
import SW.Types exposing (..)


fswtoSign : Dict.Dict String Size -> String -> Result String Sign
fswtoSign symbolsizes fsw =
    let
        laneresult =
            getFSWlane fsw

        coordinatestr =
            getcoordinatestr fsw

        coordinateresult =
            getcooordinate coordinatestr

        symbolsstrings =
            getsymbolsstrings fsw

        spellingstring =
            getspellingstring fsw

        symsresult =
            createsymbols symbolsizes symbolsstrings

        richtext =
            getrichtextstring fsw

        sign =
            Ok signinit
                |> Result.andThen
                    (setresultvalue coordinateresult (\sign value -> { sign | x = value.x, y = value.y }))
                |> Result.andThen
                    (setresultvalue laneresult (\sign value -> { sign | lane = value }))
                |> Result.andThen
                    (setresultvalue symsresult (\sign value -> { sign | syms = value }))
                |> Result.andThen
                    (setresultvalue (Ok spellingstring) (\sign value -> { sign | spelling = value }))
                |> Result.andThen
                    (\sign -> stylesign richtext sign |> Ok)
    in
        sign


stylesign : String -> Sign -> Sign
stylesign richtext sign =
    let
        stylings =
            String.split "-" richtext

        signcolors =
            getsigncolors stylings
    in
        colorsymbols sign signcolors


colorsymbols : Sign -> Colors -> Sign
colorsymbols sign colors =
    let
        symbols =
            List.map (\syms -> colorsymbol colors syms) sign.syms
    in
        { sign | syms = symbols }


colorsymbol : Colors -> Symbol -> Symbol
colorsymbol (Colors colors) symbol =
    let
        nbcolor =
            Maybe.withDefault symbol.nbcolor colors.nbcolor

        nwcolor =
            Maybe.withDefault symbol.nwcolor colors.nwcolor
    in
        { symbol | nbcolor = nbcolor, nwcolor = nwcolor }


getsigncolors : List String -> Colors
getsigncolors stylings =
    let
        secondstyling =
            stylings |> List.drop 1 |> List.head |> Maybe.withDefault ""

        cleanedstyling =
            String.slice 2 ((String.length secondstyling) - 1) secondstyling

        colorsstring =
            String.split "," cleanedstyling

        color1 =
            getcolor (colorsstring |> List.head)

        color2 =
            getcolor (colorsstring |> List.drop 1 |> List.head)

        colors =
            Colors { nbcolor = color1, nwcolor = color2 }
    in
        colors


type Colors
    = Colors { nbcolor : Maybe String, nwcolor : Maybe String }


getcolor : Maybe String -> Maybe String
getcolor colorstring =
    let
        color =
            colorstring
                |> Maybe.andThen
                    (\cstring ->
                        if testcolorrgb cstring then
                            Just ("#" ++ cstring)
                        else
                            (if cstring /= "" then
                                Just (cstring)
                             else
                                Nothing
                            )
                    )
    in
        color


createsymbols : Dict.Dict String Size -> List String -> Result String (List Symbol)
createsymbols symbolsizes symbolsstrings =
    List.map (createsymbol symbolsizes) symbolsstrings
        |> combine


createsymbol : Dict.Dict String Size -> String -> Result String Symbol
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


getFSWlane : String -> Result String Lane
getFSWlane fsw =
    let
        laneandcoordinate =
            getFSWlanestr fsw

        lanestring =
            Result.andThen (\value -> String.left 1 value |> Ok) laneandcoordinate
    in
        Result.andThen
            (\lanestringvalue ->
                getlane lanestringvalue
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



-- Get FSW


getFsw : Sign -> String
getFsw sign =
    let
        centered =
            centerSign 500 500 sign

        boundingbox =
            sign.spelling ++ "M" ++ toString (500 + (round <| toFloat centered.width / 2)) ++ "x" ++ toString (500 + (round <| toFloat centered.height / 2))

        symbols =
            List.foldr (++) "" (List.map symbolsFsw centered.syms)

        fsw =
            boundingbox ++ symbols
    in
        fsw


symbolsFsw : { c | key : String, x : a, y : b } -> String
symbolsFsw symbol =
    symbol.key ++ toString symbol.x ++ "x" ++ toString symbol.y



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


testcolorrgb : String -> Bool
testcolorrgb colorstring =
    contains (regex q_colorrgb) colorstring


getkeystr : String -> Result String String
getkeystr str =
    Regex.find (Regex.AtMost 1) (regex (re_sym)) str
        |> matchestostrings
        |> List.head
        |> Result.fromMaybe (couldnoterror "get key" str)


getspellingstring : String -> String
getspellingstring fsw =
    let
        spelling =
            Regex.find All (regex re_term) fsw
                |> matchestostrings
                |> List.head
                |> Maybe.withDefault ""
    in
        spelling


getrichtextstring : String -> String
getrichtextstring fsw =
    let
        richtextstring =
            Regex.find All (regex q_styling) fsw
                |> matchestostrings
                |> List.head
                |> Maybe.withDefault ""
    in
        richtextstring


getFSWlanestr : String -> Result String String
getFSWlanestr fsw =
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


q_styling : String
q_styling =
    "-C?(P[0-9]{2})?"
        ++ "(G_([0-9a-fA-F]{3}([0-9a-fA-F]{3})?|[a-zA-Z]+)_)?"
        ++ "(D_([0-9a-fA-F]{3}([0-9a-fA-F]{3})?|[a-zA-Z]+)(,([0-9a-fA-F]{3}([0-9a-fA-F]{3})?|[a-zA-Z]+))?_)?"
        ++ "(Z([0-9]+(.[0-9]+)?|x))?"
        ++ "(-(D[0-9]{2}_([0-9a-fA-F]{3}([0-9a-fA-F]{3})?|[a-zA-Z]+)(,([0-9a-fA-F]{3}([0-9a-fA-F]{3})?|[a-zA-Z]+))?_)*"
        ++ "(Z[0-9]{2},[0-9]+(.[0-9]+)?(,[0-9]{3}x[0-9]{3})?)*)?"
        ++ "(--?[_a-zA-Z][_a-zA-Z0-9-]{0,100}( -?[_a-zA-Z][_a-zA-Z0-9-]{0,100})*!([a-zA-Z][_a-zA-Z0-9-]{0,100}!)?)?"


q_Dstyling : String
q_Dstyling =
    "(/D_([0-9a-f]{3}([0-9a-f]{3})?|[a-zA-Z]+)(,([0-9a-f]{3}([0-9a-f]{3})?|[a-zA-Z]+))?_/)"


q_colorrgb : String
q_colorrgb =
    "/^[0-9a-fA-F]{3}([0-9a-fA-F]{3})?$/g"
