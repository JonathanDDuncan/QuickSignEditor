module SW.FSW exposing (..)

import SWEditor.EditorSign exposing (..)


toEditorSign : String -> Result String EditorSign
toEditorSign fsw =
    let
        lane =
            Ok <| getlane fsw

        x =
            Debug.log "x" <| getsignx fsw

        y =
            Debug.log "y" <| getsigny fsw

        sign =
            Ok signinit
                |> Result.andThen
                    (\sign ->
                        case x of
                            Ok xvalue ->
                                Ok { sign | x = xvalue }

                            Err msg ->
                                Err msg
                    )
                |> Result.andThen
                    (\sign ->
                        case y of
                            Ok yvalue ->
                                Ok { sign | y = yvalue }

                            Err msg ->
                                Err msg
                    )
                |> Result.andThen
                    (\sign ->
                        case lane of
                            Ok lanevalue ->
                                Ok { sign | lane = lanevalue }

                            Err msg ->
                                Err msg
                    )
    in
        sign


getlane fsw =
    let
        laneandcoordinate =
            getlaneandcoordinate fsw

        lanestring =
            String.left 1 laneandcoordinate
    in
        List.filter (\( value, lane ) -> value == lanestring) lanes
            |> List.map (\( value, lane ) -> lane)
            |> List.head
            |> Maybe.withDefault MiddleLane


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
                    Err <| "Could not get y of " ++ fsw ++ "|" ++ msg
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
                    Err <| "Could not get y of " ++ fsw ++ "|" ++ msg
    in
        y


getcoordinatelist : String -> Result String (List String)
getcoordinatelist fsw =
    let
        laneandcoordinate =
            getlaneandcoordinate fsw

        coordinatestring =
            String.dropLeft 1 laneandcoordinate

        coordinatelist =
            String.split "x" coordinatestring
    in
        if String.length coordinatestring == 7 then
            Ok coordinatelist
        else
            Err <| "Sign coordinate " ++ coordinatestring ++ "should be 7 characters long"


toValue : Maybe String -> Int
toValue str =
    str
        |> Maybe.withDefault "0"
        |> String.toInt
        |> Result.withDefault 0


getlaneandcoordinate : String -> String
getlaneandcoordinate fsw =
    let
        symbolsplit =
            String.split "S" fsw
    in
        List.filter (\item -> startwithlanevalue item) symbolsplit
            |> List.head
            |> Maybe.withDefault ""


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
