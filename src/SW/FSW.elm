module SW.FSW exposing (..)

import SWEditor.EditorSign exposing (..)


toEditorSign : String -> Result String EditorSign
toEditorSign fsw =
    let
        laneresult =
            getlane fsw

        xresult =
            getsignx fsw

        yresult =
            getsigny fsw

        sign =
            Ok signinit
                |> Result.andThen
                    (setresultvalue xresult (\sign value -> { sign | x = value }))
                |> Result.andThen
                    (setresultvalue yresult (\sign value -> { sign | y = value }))
                |> Result.andThen
                    (setresultvalue laneresult (\sign value -> { sign | lane = value }))
    in
        sign


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


getsignx : String -> Result String Int
getsignx fsw =
    let
        coordinatelistresult =
            getcoordinatelist fsw
    in
        applyToOkValueAppendMsg ("Could not get x of '" ++ fsw ++ "'")
            (\value ->
                value
                    |> List.head
                    |> toValueDefaultZero
            )
            coordinatelistresult


getsigny : String -> Result String Int
getsigny fsw =
    let
        coordinatelistresult =
            getcoordinatelist fsw
    in
        applyToOkValueAppendMsg ("Could not get x of '" ++ fsw ++ "'")
            (\value ->
                value
                    |> List.drop 1
                    |> List.head
                    |> toValueDefaultZero
            )
            coordinatelistresult


getcoordinatelist : String -> Result String (List String)
getcoordinatelist fsw =
    let
        laneandcoordinate =
            getlaneandcoordinate fsw

        coordinatestring =
            applyToOkValue (String.dropLeft 1) laneandcoordinate

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
            String.split "S" fsw
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
