module SW.Pua
    exposing
        ( Base
        , Fill
        , Rotation
        , Key
        , Code
        , createkey
        , puaCharCode
        , linecodefromkey
        , fillcodefromkey
        , codefromkey
        , getvalidfills
        , isValidRotation
        , getvalidrotations
        , iskey
        , ishand
        )

import ParseInt as ParseInt exposing (parseIntHex)
import String as String exposing (slice)
import Char as Char
import Bitwise as Bitwise


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


linecode : Base -> Fill -> Rotation -> Int
linecode base fill rotation =
    0x00040000 + 96 * (base - 256) + 16 * fill + rotation + 1


linecodefromkey : Key -> Int
linecodefromkey key =
    linecode (hexbaseFromKey key) (hexfillFromKey key) (hexrotationFromKey key)



-- fillcode also named code


fillcode : Base -> Fill -> Rotation -> Int
fillcode base fill rotation =
    0x00100000 + 96 * (base - 256) + 16 * fill + rotation + 1


fillcodefromkey : Key -> Int
fillcodefromkey key =
    fillcode (hexbaseFromKey key) (hexfillFromKey key) (hexrotationFromKey key)


createkey : Base -> Fill -> Rotation -> Key
createkey base fill rotation =
    "S" ++ String.toLower (dectoHex base ++ dectoHex (fill - 1) ++ dectoHex (rotation - 1))


dectoHex : Int -> String
dectoHex value =
    case ParseInt.toRadix 16 value of
        Err _ ->
            ""

        Ok str ->
            str


hextoDec : String -> Int
hextoDec value =
    ParseInt.parseIntHex value |> Result.toMaybe |> Maybe.withDefault 0


hexbaseFromKey : Key -> Base
hexbaseFromKey createkey =
    hextoDec (slice 1 4 createkey)


hexfillFromKey : Key -> Fill
hexfillFromKey createkey =
    hextoDec (slice 4 5 createkey)


hexrotationFromKey : Key -> Rotation
hexrotationFromKey createkey =
    hextoDec (slice 5 6 createkey)


codefromkey : Key -> Code
codefromkey createkey =
    fillcode (hexbaseFromKey createkey) (hexfillFromKey createkey) (hexrotationFromKey createkey)


puaCharCode : Int -> String
puaCharCode value =
    String.fromChar (Char.fromCode <| lefthalf value) ++ String.fromChar (Char.fromCode <| righthalf value)


lefthalf : Int -> Int
lefthalf value =
    hextoDec "D800" + Bitwise.shiftRightBy 10 (value - hextoDec "10000")


righthalf : Int -> Int
righthalf value =
    hextoDec "DC00" + Bitwise.and (value - hextoDec "10000") (hextoDec "03FF")


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
                _ =
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


type alias TypeRange =
    { start : String
    , end : String
    }
