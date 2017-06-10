module SW.Symbol
    exposing
        ( Symbol
        , symbolinit
        , Base
        , Fill
        , getvalidfills
        , Rotation
        , isValidRotation
        , getvalidrotations
        , Key
        , iskey
        , ishand
        , Code
        , HandFills(..)
        , Hands(..)
        , Planes(..)
        , SymbolSize
        , gethandtype
        , moveSymbols
        , moveSymbol
        )

import ParseInt exposing (parseIntHex)


type alias Symbol =
    { key : String
    , id : Int
    , selected : Bool
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    , size : Float
    , nwcolor : String
    , nbcolor : String
    }


symbolinit : Symbol
symbolinit =
    { key = ""
    , id = 0
    , selected = False
    , x = 0
    , y = 0
    , width = 0
    , height = 0
    , size = 1
    , nwcolor = ""
    , nbcolor = ""
    }


type alias Base =
    Int


type alias Fill =
    Int


type alias Rotation =
    Int


type alias Code =
    Int


type alias Key =
    String


type alias TypeRange =
    { start : String
    , end : String
    }


iskey : String -> String -> Bool
iskey key typename =
    let
        range =
            typerange typename

        start =
            parseIntHex range.start
                |> Result.toMaybe
                |> Maybe.withDefault 0

        end =
            parseIntHex range.end
                |> Result.toMaybe
                |> Maybe.withDefault 0

        char =
            String.slice 1 4 key
                |> parseIntHex
                |> Result.toMaybe
                |> Maybe.withDefault 0
    in
        start <= char && end >= char


ishand : String -> Bool
ishand key =
    iskey key "hand"



--iskey "S100" "hand" -> True
--iskey "S100" "head" -> False


getvalidfills : String -> List Fill
getvalidfills validfillsstring =
    case validfillsstring of
        "1 - 6" ->
            List.range 1 6

        "1 - 4" ->
            List.range 1 4

        "1, 2" ->
            List.range 1 2

        "1 - 3" ->
            List.range 1 3

        "1 - 5" ->
            List.range 1 5

        "1" ->
            [ 1 ]

        "2" ->
            [ 2 ]

        _ ->
            let
                _ =
                    Debug.log "Could not match valid fills string" validfillsstring
            in
                []


getvalidrotations : String -> List Rotation
getvalidrotations validrotationsstring =
    case validrotationsstring of
        "1 - 16" ->
            List.range 1 16

        "1 - 8" ->
            List.range 1 8

        "1" ->
            [ 1 ]

        "1 - 4" ->
            List.range 1 4

        "1, 2, 4, 5, 6, 8" ->
            [ 1, 2, 4, 5, 6, 8 ]

        "1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 14, 16" ->
            [ 1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 14, 16 ]

        "1 - 6" ->
            List.range 1 6

        "1, 2" ->
            List.range 1 2

        "1 - 9" ->
            List.range 1 9

        _ ->
            let
                a =
                    Debug.log "Could not match valid rotations string" validrotationsstring
            in
                []


isValidRotation : Int -> List Int -> Bool
isValidRotation rotation validrotations =
    List.any ((==) rotation) validrotations


typerange : String -> TypeRange
typerange typename =
    case typename of
        "writing" ->
            { start = "100"
            , end = "37e"
            }

        "hand" ->
            { start = "100"
            , end = "204"
            }

        "movement" ->
            { start = "205"
            , end = "2f6"
            }

        "dynamic" ->
            { start = "2f7"
            , end = "2fe"
            }

        "head" ->
            { start = "2ff"
            , end = "36c"
            }

        "hcenter" ->
            { start = "2ff"
            , end = "36c"
            }

        "vcenter" ->
            { start = "2ff"
            , end = "375"
            }

        "trunk" ->
            { start = "36d"
            , end = "375"
            }

        "limb" ->
            { start = "376"
            , end = "37e"
            }

        "location" ->
            { start = "37f"
            , end = "386"
            }

        "punctuation" ->
            { start = "387"
            , end = "38b"
            }

        _ ->
            { start = "100"
            , end = "38b"
            }


type HandFills
    = LeftBack
    | LeftThumbEdge
    | LeftPalm
    | LeftBabyEdge
    | RightBack
    | RightThumbEdge
    | RightPalm
    | RightBabyEdge


type Hands
    = Right
    | Left


type Planes
    = Wall
    | Floor


gethandtype : HandFills -> Hands
gethandtype filltype =
    case filltype of
        LeftBack ->
            Left

        LeftThumbEdge ->
            Left

        LeftPalm ->
            Left

        LeftBabyEdge ->
            Left

        RightBack ->
            Right

        RightThumbEdge ->
            Right

        RightPalm ->
            Right

        RightBabyEdge ->
            Right


type alias SymbolSize =
    { k : String
    , h : Int
    , w : Int
    }


moveSymbols : Int -> Int -> List Symbol -> List Symbol
moveSymbols movex movey symbols =
    List.map (moveSymbol movex movey) symbols


moveSymbol : Int -> Int -> Symbol -> Symbol
moveSymbol movex movey symbol =
    { symbol | x = symbol.x + movex, y = symbol.y + movey }
