module SW.Pua exposing (key, puaCharCode, linecodefromkey, fillcodefromkey, codefromkey)

import ParseInt as ParseInt exposing (toRadix, parseIntHex)
import String as String exposing (..)
import Char as Char exposing (..)
import Bitwise as Bitwise exposing (..)
import SW.Types exposing (Base, Fill, Rotation, Key, Code)


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


key : Base -> Fill -> Rotation -> Key
key base fill rotation =
    "S" ++ String.toLower (dectoHex base ++ dectoHex (fill - 1) ++ dectoHex (rotation - 1))


dectoHex : Int -> String
dectoHex value =
    let
        val =
            ParseInt.toRadix 16 value
    in
        case val of
            Err msg ->
                ""

            Ok str ->
                str


hextoDec : String -> Int
hextoDec value =
    ParseInt.parseIntHex value |> Result.toMaybe |> Maybe.withDefault 0


hexbaseFromKey : Key -> Base
hexbaseFromKey key =
    hextoDec (slice 1 4 key)


hexfillFromKey : Key -> Fill
hexfillFromKey key =
    hextoDec (slice 4 5 key)


hexrotationFromKey : Key -> Rotation
hexrotationFromKey key =
    hextoDec (slice 5 6 key)


codefromkey : Key -> Code
codefromkey key =
    fillcode (hexbaseFromKey key) (hexfillFromKey key) (hexrotationFromKey key)


puaCharCode : Int -> String
puaCharCode value =
    String.fromChar (Char.fromCode <| lefthalf value) ++ String.fromChar (Char.fromCode <| righthalf value)


lefthalf : Int -> Int
lefthalf value =
    hextoDec "D800" + Bitwise.shiftRightBy 10 (value - hextoDec "10000")


righthalf : Int -> Int
righthalf value =
    hextoDec "DC00" + Bitwise.and (value - hextoDec "10000") (hextoDec "03FF")
