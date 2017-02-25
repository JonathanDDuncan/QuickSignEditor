module SW.FSW exposing (..)

import SWEditor.EditorSign exposing (..)


toEditorSign : String -> EditorSign
toEditorSign fsw =
    let
        lane =
            getlane fsw

        x =
            getsignx fsw

        y =
            getsigny fsw
    in
        { signinit
            | x = x
            , y = y
            , lane = lane
        }


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


getsignx : String -> Int
getsignx fsw =
    let
        coordinatelist =
            getcoordinatelist fsw

        x =
            coordinatelist
                |> List.head
                |> toValue
    in
        x


getsigny : String -> Int
getsigny fsw =
    let
        coordinatelist =
            getcoordinatelist fsw

        y =
            List.drop 1 coordinatelist
                |> List.head
                |> toValue
    in
        y


getcoordinatelist : String -> List String
getcoordinatelist fsw =
    let
        laneandcoordinate =
            getlaneandcoordinate fsw

        coordinatestring =
            String.dropLeft 1 laneandcoordinate

        coordinatelist =
            String.split "x" coordinatestring
    in
        coordinatelist


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
