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


setresultvalue result setter =
    (\recd ->
        case result of
            Ok value ->
                Ok (setter recd value)

            Err msg ->
                Err msg
    )


getlane fsw =
    let
        laneandcoordinate =
            getlaneandcoordinate fsw

        lanestring =
            case laneandcoordinate of
                Ok value ->
                    Ok <| String.left 1 value

                Err msg ->
                    Err msg
    in
        case lanestring of
            Ok lanestringvalue ->
                List.filter (\( value, lane ) -> value == lanestringvalue) lanes
                    |> List.map (\( value, lane ) -> lane)
                    |> List.head
                    |> Maybe.withDefault MiddleLane
                    |> Ok

            Err msg ->
                Err msg


getsignx : String -> Result String Int
getsignx fsw =
    let
        coordinatelistresult =
            getcoordinatelist fsw

        x =
            case coordinatelistresult of
                Ok coordinatelist ->
                    coordinatelist
                        |> List.head
                        |> toValue
                        |> Ok

                Err msg ->
                    Err <| "Could not get y of '" ++ fsw ++ "'|" ++ msg
    in
        x


getsigny : String -> Result String Int
getsigny fsw =
    let
        coordinatelistresult =
            getcoordinatelist fsw

        y =
            case coordinatelistresult of
                Ok coordinatelist ->
                    List.drop 1 coordinatelist
                        |> List.head
                        |> toValue
                        |> Ok

                Err msg ->
                    Err <| "Could not get y of '" ++ fsw ++ "'|" ++ msg
    in
        y


getcoordinatelist : String -> Result String (List String)
getcoordinatelist fsw =
    let
        laneandcoordinate =
            getlaneandcoordinate fsw

        coordinatestring =
            case laneandcoordinate of
                Ok value ->
                    Ok <| String.dropLeft 1 value

                Err msg ->
                    Err msg

        goodcoordinatestring =
            case coordinatestring of
                Ok value ->
                    if String.length value == 7 then
                        Ok value
                    else
                        Err <| "Sign coordinate '" ++ value ++ "'should be 7 characters long."

                Err msg ->
                    Err msg

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


toValue : Maybe String -> Int
toValue str =
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


lanes =
    [ ( "B", BLane )
    , ( "L", LeftLane )
    , ( "M", MiddleLane )
    , ( "R", RightLane )
    ]
