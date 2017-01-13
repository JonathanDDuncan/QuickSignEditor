module SW.SymbolConverter exposing (..)

import ParseInt as ParseInt exposing (..)
import String as String exposing (..)
import Char as Char exposing (..)
import Bitwise as Bitwise exposing (..)
import SW.Types exposing (..)


rotation : Int -> Int
rotation code =
    (code - 0x00100000) % 16


fill : Int -> Int
fill code =
    ((code - 0x00100000) // 16) % 6 + 1


base : Int -> Int
base code =
    ((code - 0x00100000) // 96) + 256


code : Base -> Fill -> Rotation -> Code
code base fill rotation =
    0x00100000 + 96 * (base - 256) + 16 * (fill) + rotation + 1


key : Base -> Fill -> Rotation -> Key
key base fill rotation =
    "S" ++ dectoHex base ++ dectoHex (fill - 1) ++ dectoHex (rotation - 1)


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


keyfromcode : Code -> Key
keyfromcode code =
    key (base code) (fill code) (rotation code)


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
    code (hexbaseFromKey key) (hexfillFromKey key) (hexrotationFromKey key)


puabasestart : Int
puabasestart =
    hextoDec "FD730"


puafillstart : Int
puafillstart =
    hextoDec "FD810"


puarotationstart : Int
puarotationstart =
    hextoDec "FD820"


pua : String -> String
pua key =
    let
        base =
            hexbaseFromKey key

        puabase =
            base + puabasestart

        fill =
            hexfillFromKey key

        puafill =
            fill + puafillstart

        rotation =
            hexrotationFromKey key

        puarotation =
            rotation + puarotationstart
    in
        puaCharCode (puabase) ++ puaCharCode (puafill) ++ puaCharCode (puarotation)


puahextext : String -> String
puahextext key =
    let
        base =
            (hexbaseFromKey key)

        puabase =
            base + puabasestart

        fill =
            hexfillFromKey key

        puafill =
            fill + puafillstart

        rotation =
            hexrotationFromKey key

        puarotation =
            rotation + puarotationstart
    in
        dectoHex puabase ++ dectoHex puafill ++ dectoHex puarotation


lefthalf : Int -> Int
lefthalf value =
    hextoDec "D800" + (Bitwise.shiftRightBy 10 (value - (hextoDec "10000")))


righthalf : Int -> Int
righthalf value =
    hextoDec "DC00" + (Bitwise.and (value - (hextoDec "10000")) (hextoDec "03FF"))


puaCharCode : Int -> String
puaCharCode value =
    String.fromChar (Char.fromCode <| lefthalf value) ++ String.fromChar (Char.fromCode <| righthalf value)
